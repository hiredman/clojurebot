(ns hiredman.utilities
  (:use (hiredman horizon))
  (:import (java.net URL URLEncoder)
           (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (java.text SimpleDateFormat ParsePosition)
           (sun.misc BASE64Encoder)))

(defn get-url [x]
  (with-open [a (-> (doto (-> x URL. .openConnection)
                      (.setRequestProperty "User-Agent" "clojurebot")
                      (.setRequestProperty "Accept" "application/xml"))
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

(defn tinyurl [url]
  (-> "http://is.gd/api.php?longurl=%s" (format (URLEncoder/encode url))
      get-url))

(def tinyurl (memoize tinyurl))

(defn- base64encode [string]
  (.trim (.encode (BASE64Encoder.) (.getBytes string))))

(defn date [string format]
  (.parse (SimpleDateFormat. format) string (ParsePosition. 0)))

(defmacro with-breaker [seconds & body]
  `(let [f# (future
              ~@body)]
     (try
       (.get f# ~seconds java.util.concurrent.TimeUnit/SECONDS)
       (catch Throwable t#
         (future-cancel f#)
         (throw t#)))))
