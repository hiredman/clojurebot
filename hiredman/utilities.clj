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
