(ns hiredman.clojurebot.code-lookup
    (:use (hiredman.clojurebot core))
    (:use (hiredman utilities))
    (:import (java.io File InputStreamReader BufferedReader)))

(def google-code-url
     "http://code.google.com/p/clojure/source/browse/trunk/src/clj/clojure/")

(defn get-rev-number []
      ((comp #(Integer/parseInt %)
             second
             #(.split % " ")
             first 
             (partial filter #(re-find #"^Revision: " %))
             line-seq
             #(BufferedReader. %)
             #(InputStreamReader. %)
             #(.getInputStream %)
             #(.. Runtime getRuntime (exec % %2 %3)))
       (into-array ["svn" "info"])
       nil
       (File. "/home/hiredman/clojure/")))

(def clojurebot-rev (get-rev-number))

(defn get-file-and-ln [string]
      (let [a (meta (resolve (symbol string)))]
        [(:line a) (:file a)]))

(defn make-url [[line file]]
      (let [google (str google-code-url file "?=" clojurebot-rev "#" line)
            google (java.net.URLEncoder/encode google)
            url (str "http://tinyurl.com/api-create.php?url=" google)]
        (get-url url)))

(def make-url-cached (memoize make-url))

(defmethod responder ::code-lookup [bot msg]
  (let [message (extract-message bot msg)
        thing (second (.split message " "))]
    (send-out :notice bot (who msg) (str thing ": " (make-url-cached (get-file-and-ln thing))))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^def " (extract-message bot msg)))) ::code-lookup)
