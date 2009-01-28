(ns hiredman.clojurebot.google
  (:use (hiredman.clojurebot core))
  (:require [org.danlarkin.json :as json]))


(defn get-url [x]
      (with-open [a (java.io.DataInputStream. (.getContent (java.net.URL. x)))]
                 (.readLine a)))

(defn google [term]
      (json/decode-from-str (get-url (str "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=" (java.net.URLEncoder/encode term)))))

(defn cull [result-set]
      [(:estimatedResultCount (:cursor (:responseData result-set)))
       (first (:results (:responseData result-set)))])

(defmethod responder ::google [bot msg]
  (let [term (.trim (.replaceFirst (extract-message bot msg) "^google " ""))]
    (let [[num result] (cull (google term))]
      (sendMsg-who bot msg (str "First, out of " num " results is:"))
      (sendMsg-who bot msg (:titleNoFormatting result))
      (sendMsg-who bot msg (:unescapedUrl result)))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^google " (extract-message bot msg)))) ::google)
