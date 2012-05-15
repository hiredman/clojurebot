(ns hiredman.factoid-server
  (:use [hiredman.http-server :only (http-server put)]
        [hiredman.triples :only (derby query)]
        [hiredman.clojurebot.storage :only (db-name)])
  (:require [cheshire.core :as json])
  (:import (java.io BufferedReader StringReader)))

(defn handle [request bot]
  (-> request StringReader. BufferedReader. line-seq
    ((partial map #(.trim %)))
    ((partial drop-while #(not= "" %)))
    rest
    ((partial apply str))
    (json/decode true)
    ((fn [x]
       (query (derby (db-name bot))
              (:s x :x)
              (:p x :y)
              (:o x :z))))
    ((partial sort-by :upper_subject))
    ((partial map #(update-in % [:created_at] (fn [x] (.toString x)))))
    ((fn [x]
       (try
         (json/encode x true)
         (catch Exception e
           (print e)))))
    ((fn [x]
      {:status "200 OK"
       :content-type "application/json"
       :body x}))))

(defn f [r bot]
  (if (zero? (.available r)) (Thread/sleep 100))
  (let [l (.available r)
        buf (make-array Byte/TYPE l)]
    (.read r buf)
    (let [request (String. buf)]
      (prn request)
      (cond
        (.startsWith request "PUT")
          (handle request bot)
        :else
          {:status "404 Not Found" :content-type "text/plain" :body "hello world"}))))

(defn factoid-server-fn [bot]
  (fn [r] (f r bot)))

(defn factoid-server [port bot]
  (http-server (factoid-server-fn bot) port)
  bot)
