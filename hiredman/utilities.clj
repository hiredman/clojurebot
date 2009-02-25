(ns hiredman.utilities
    (:use (hiredman horizon))
    (:import (java.net URL)
             (java.io BufferedReader InputStreamReader)))

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
