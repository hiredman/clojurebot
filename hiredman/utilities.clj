(ns hiredman.utilities
    (:use (hiredman horizon))
    (:import (java.net URL URLEncoder)
             (java.io BufferedReader InputStreamReader OutputStreamWriter)
			 (sun.misc BASE64Encoder)))

(defn get-url [x]
      (with-open [a (java.io.BufferedReader. (java.io.InputStreamReader. (.getContent (java.net.URL. x))))]
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

(defn tweet [user pw status] 
      (shell (str "curl -u "user":"pw" -d status=" (URLEncoder/encode status) " http://twitter.com/statuses/update.xml")))
