(ns hiredman.clojurebot.translate
    (:user (hiredman.clojurebot core))
    (:require [org.danlarkin.json :as json]))

(require '[org.danlarkin.json :as json])

(def url "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0")

(defn get-url [x]
      (with-open [a (java.io.DataInputStream. (.getContent (java.net.URL. x)))]
                 (.readLine a)))

(defn translate [string]
      (let [[direction lang phrase] (rest (re-matches #"^translate (from|to) ([a-z]+):(.*)" string))
            langpair (condp = direction
                            "to"
                              (java.net.URLEncoder/encode (str "en|" lang))
                             "from"
                              (java.net.URLEncoder/encode (str lang "|en")))
            phrase (java.net.URLEncoder/encode (.trim phrase))]
        ((comp :translatedText :responseData json/decode-from-str) (get-url (str url "&q=" phrase "&langpair=" langpair)))))

(defmethod responder ::translate [bot msg]
  (sendMsg-who bot msg (str (translate (extract-message bot msg)))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^translate " (extract-message bot msg)))) ::translate)
