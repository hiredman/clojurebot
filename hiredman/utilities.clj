(ns hiredman.utilities
    (:use (hiredman horizon))
    (:import (java.net URL URLEncoder)
             (java.io BufferedReader InputStreamReader OutputStreamWriter)
             (sun.misc BASE64Encoder)))

(defn get-url [x]
      (with-open [a (-> (doto (-> x URL. .openConnection)
                              (.setRequestProperty "User-Agent" "clojurebot"))
                        .getInputStream InputStreamReader. BufferedReader.)]
        (loop [buf (StringBuilder.) line (.readLine a)]
          (if line
            (recur (doto buf (.append line)) (.readLine a))
            (.toString buf)))))

(defmacro mk-interface [class fn]
  (let [x (map #(list (symbol (.getName %))
                      ['& 'x]
                      (list 'let ['method (keyword (.getName %))] fn)) (.getMethods (eval class)))]
    `(proxy [~class] [] ~@x)))

(defn scoped-get-url [x]
      (let [t (-> x URL. .getContent InputStreamReader. BufferedReader.)]
        (hiredman.horizon/when-hrz :exits #(.close t))
        t))

(defn shell [cmd]
      (.. Runtime getRuntime (exec cmd)))

(def #^{:private true} twit (java.util.concurrent.LinkedBlockingQueue.))

(def #^{:private true} twitter-urls {:update (URL. "http://twitter.com/statuses/update.xml")})

(defn- base64encode [string]
  (.trim (.encode (BASE64Encoder.) (.getBytes string))))

;(def a (let [a (atom 0)]
;         (proxy [clojure.lang.AFn clojure.lang.IDeref] []
;         (invoke []
;                 (swap! a inc)
;                 @a)
;         (deref [] a))))

(defn tweet-send [username password text]
  (let [creds (base64encode (str username ":" password))
        con (doto (.openConnection (:update twitter-urls))
              (.setDoInput true) (.setDoOutput true) (.setUseCaches false)
              (.setRequestProperty "Authorization" (str "Basic " creds))
              (.setRequestProperty "User-Agent" "clojurebot 10/10"))
        status (str "status=" (URLEncoder/encode text "UTF-8"))]
    (with-open [wrt (-> con .getOutputStream OutputStreamWriter.)]
                (.write wrt status))
    (with-open [rdr (-> con .getInputStream InputStreamReader. BufferedReader.)]
                (apply str (line-seq rdr)))))

(defn start-twitter [user passwd & tags]
      (send-off (agent nil)
                (fn this [& _]
                    (when-let [status (.take twit)]
                        (tweet-send user passwd (let [x (apply str status " " tags)]
                                                  (if (<= 140 (count (.getBytes x)))
                                                    x
                                                    status))))
                    (Thread/sleep 600000)
                    (send-off *agent* this))))



(defn tweet [status]
      (println "tweet |" status)
      (.offer twit status))
