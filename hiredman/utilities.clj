(ns hiredman.utilities
    (:use (hiredman horizon))
    (:import (java.net URL URLEncoder)
             (java.io BufferedReader InputStreamReader OutputStreamWriter)
             (sun.misc BASE64Encoder)))

(defn get-url [x]
      (with-open [a (-> (doto (-> x URL. .openConnection)
                              (.setRequestProperty "User-Agent" "clojurebot"))
                        .getInputStream InputStreamReader. BufferedReader.)]
                 (.readLine a)))

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

(defn tweet-send [user pw status] 
      (shell (str "curl -u "user":"pw" -d status=" (URLEncoder/encode status) " http://twitter.com/statuses/update.xml")))

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
