(ns hiredman.twitter
  (:refer-clojure :exclude [send])
  (:require [org.danlarkin.json :as json]
            [clojure.core :as cc]
            [hiredman.sandbox :as sb])
  (:import (java.net URL URLEncoder)
           (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (sun.misc BASE64Encoder)))

(def #^{:private true} twit (java.util.concurrent.LinkedBlockingQueue.))

(def #^{:private true} twitter-urls {:update (URL. "http://twitter.com/statuses/update.xml")
                                     :messages (URL. "http://twitter.com/direct_messages.json")
                                     :mentions (URL. "http://twitter.com/statuses/mentions.json")})

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
                (json/decode-from-str (apply str (line-seq rdr))))))



(defn format-mentions [x]
  {:text (:text x) :name (:screen_name (:user x)) :id (:id x)})

(defn dwd [tweet]
  (try
    (let [x (sb/eval-in-box (.trim (.replaceAll (:text tweet) "^@clojurebot:?" "")) 'sandbox)]
      (if (x 0)
        (x 0)
        (x 2)))
    (catch Exception e
      (str "Eval-in-box threw an exception:" (.getMessage e)))))

(defn twitter-repl [a & _]
  (let [mentions (filter #(re-find #"^@clojurebot" (:text %))
                         (sort-by :id
                                  (map format-mentions
                                       (get-mentions  a))))
        last_m (:id (last mentions))]
    (doall (map (fn [tw] (tweet (str "@" (:name tw) " " (dwd tw)))) mentions))
    (Thread/sleep (* 60000 1))
    (send-off *agent* twitter-repl)
    last_m))

(defn twitter-repl [a & _]
  nil)

;(def twagent (agent 1606137842))

;(send-off twagent twitter-repl)
