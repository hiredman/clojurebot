(ns hiredman.twitter
  (:refer-clojure :exclude [send])
  (:require [clojure.core :as cc])
  (:import (java.net URL URLEncoder)
           (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (sun.misc BASE64Encoder)))

(def #^{:private true} twit (java.util.concurrent.LinkedBlockingQueue.))

(def #^{:private true} twitter-urls {:update (URL. "http://twitter.com/statuses/update.xml")})

(defn- base64encode [string] (.trim (.encode (BASE64Encoder.) (.getBytes string))))

(defn send [username password text]
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
                        (send user passwd (let [x (apply str status " " tags)]
                                                  (if (<= 140 (count (.getBytes x)))
                                                    x
                                                    status))))
                    (Thread/sleep 600000)
                    (send-off *agent* this))))

(defn tweet [status] (io! (.offer twit status)))
