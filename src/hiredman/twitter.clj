(ns hiredman.twitter
  (:refer-clojure :exclude [send])
  (:require [org.danlarkin.json :as json]
            [clojure.core :as cc]
            [hiredman.clojurebot.core :as core]
            [hiredman.sandbox :as sb])
  (:import (java.net URL URLEncoder)
           (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (sun.misc BASE64Encoder)))

(def #^{:private true} twit (java.util.concurrent.LinkedBlockingQueue.))

(def #^{:private true} twitter-urls {:update (URL. "http://twitter.com/statuses/update.xml")
                                     :messages (URL. "http://twitter.com/direct_messages.json")
                                     :mentions (URL. "http://twitter.com/statuses/mentions.json")})

(defstruct account :username :password)

(defn- base64encode [string] (.trim (.encode (BASE64Encoder.) (.getBytes string))))

(def tweet-a false)

(defn send [username password text]
  (when tweet-a
    (let [creds (base64encode (str username ":" password))
          con (doto (.openConnection (:update twitter-urls))
                (.setDoInput true) (.setDoOutput true) (.setUseCaches false)
                (.setRequestProperty "Authorization" (str "Basic " creds))
                (.setRequestProperty "User-Agent" "clojurebot 10/10"))
          status (str "status=" (URLEncoder/encode text "UTF-8"))]
      (with-open [wrt (-> con .getOutputStream OutputStreamWriter.)]
                  (.write wrt status))
      (with-open [rdr (-> con .getInputStream InputStreamReader. BufferedReader.)]
                  (apply str (line-seq rdr))))))

(defn get-api [call username password]
  (let [creds (base64encode (str username ":" password))
        con (doto (.openConnection (call twitter-urls))
              (.setUseCaches false)
              (.setRequestProperty "Authorization" (str "Basic " creds))
              (.setRequestProperty "User-Agent" "clojurebot 10/10"))]
    (with-open [rdr (-> con .getInputStream InputStreamReader. BufferedReader.)]
                (json/decode-from-str (apply str (line-seq rdr))))))

(defn open-connection [url creds]
  (doto (.openConnection url)
    (.setDoInput true) (.setDoOutput true) (.setUseCaches false)
    (.setRequestProperty "Authorization" (str "Basic " creds))
    (.setRequestProperty "User-Agent" "clojurebot 10/10")))

(defn get-mentions [username password & id]
  (let [creds (base64encode (str username ":" password))
        con (open-connection (if (nil? id)
                               (:mentions twitter-urls)
                               (URL. (str (:mentions twitter-urls) "?since_id=" (first id)))) creds)]
    (with-open [rdr (-> con .getInputStream InputStreamReader. BufferedReader.)]
      ;(take-while identity (repeatedly #(.readLine rdr)))
      (json/decode-from-str (apply str (line-seq rdr))))))


(defn login [username password]
  (vary-meta (struct account username password) assoc :type ::twitter))

(defmethod core/new-send-out ::twitter [acct _ thing message]
  (when (not= "" message)
    (send (:username acct) (:password acct) (str "@" (:sender thing) " " message))))

(defn tweet [x]
  (vary-meta (struct core/junks nil (-> x :user :screen_name) nil nil
                     (.replaceAll (:text x) "^@clojurebot " ""))
             assoc :addressed? true))

(def running true)

(defn twitter-loop [_ acct]
  (let [m (get-mentions (:username acct) (:password acct) (:x (meta acct)))]
    (doseq [i m]
      (try (trampoline core/responder acct (tweet i))
        (catch Exception e
          (.printStackTrace e))))
    (when running
      (send-off *agent* twitter-loop (vary-meta acct assoc :x (:id (last (sort-by :id m)))))
      (Thread/sleep 100000))))
