;; DEPENDS clojure-json
(ns hiredman.clojurebot.google
  (:use (hiredman.clojurebot core))
  (:use (hiredman utilities))
  (:require [clojurebot.json :as json]))

(def lmgtfy "http://lmgtfy.com/?q=")

(def wheel 100)

(defn google [term]
      (json/decode-from-str (get-url (str "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" (java.net.URLEncoder/encode term)))))

(defn cull [result-set]
      [(:estimatedResultCount (:cursor (:responseData result-set)))
       (first (:results (:responseData result-set)))])

(defn google-search? [{:keys [message]}]
  (and message
       (re-find #"^google " message)))

(defn google-search [{:keys [message]}]
  (let [term (.trim (.replaceFirst message "^google " ""))]
    (if (= 0 (rand-int wheel))
      (str lmgtfy (java.net.URLEncoder/encode term))
      (let [[num result] (cull (google term))]
        (format "%s\n%s\n%s"
                (str "First, out of " num " results is:")
                (:titleNoFormatting result)
                (:unescapedUrl result))))))
