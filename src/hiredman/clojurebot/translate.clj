(ns hiredman.clojurebot.translate
    (:use (hiredman.clojurebot core))
    (:use (hiredman utilities))
    (:require [clojurebot.json :as json]))

(def url "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0")

(defn translate [string]
      (let [[direction lang phrase] (rest (re-matches #"^translate (from|to) ([a-z]+)(?:\:|\ )(.*)" string))
            langpair (condp = direction
                            "to"
                              (java.net.URLEncoder/encode (str "en|" lang) "UTF-8")
                             "from"
                              (java.net.URLEncoder/encode (str lang "|en") "UTF-8"))
            phrase (java.net.URLEncoder/encode (.trim #^String phrase))]
        (println langpair)
        ((comp #(java.net.URLDecoder/decode % "UTF-8")
               :translatedText :responseData
               json/decode-from-str
               #(do (prn %) %)) (get-url (str url "&q=" phrase "&langpair=" langpair)))))

(defmethod responder ::translate [bot msg]
  (sendMsg-who bot msg (str (translate (extract-message bot msg)))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^translate " (extract-message bot msg)))) ::translate)
