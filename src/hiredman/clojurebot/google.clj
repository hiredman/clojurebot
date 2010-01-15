;; DEPENDS clojure-json
(ns hiredman.clojurebot.google
  (:use (hiredman.clojurebot core))
  (:use (hiredman utilities))
  (:require [org.danlarkin.json :as json]))

(def lmgtfy "http://lmgtfy.com/?q=")

(def wheel 100)

(defn google [term]
      (json/decode-from-str (get-url (str "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" (java.net.URLEncoder/encode term)))))

(defn cull [result-set]
      [(:estimatedResultCount (:cursor (:responseData result-set)))
       (first (:results (:responseData result-set)))])

(defmethod responder ::google [bot msg]
  (let [term (.trim (.replaceFirst (extract-message bot msg) "^google " ""))]
    (if (= 0 (rand-int wheel))
      (new-send-out bot :msg msg (str lmgtfy (java.net.URLEncoder/encode term)))
      (let [[num result] (cull (google term))]
        (new-send-out bot :msg msg (str "First, out of " num " results is:"))
        (new-send-out bot :msg msg (:titleNoFormatting result))
        (new-send-out bot :msg msg (:unescapedUrl result))))))

(add-dispatch-hook (dfn (and (:addressed? (meta msg))
                             (re-find #"^google " (extract-message bot msg)))) ::google)
