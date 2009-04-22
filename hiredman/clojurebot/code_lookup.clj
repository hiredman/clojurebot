(ns hiredman.clojurebot.code-lookup
    (:use (hiredman.clojurebot core))
    (:use (hiredman utilities))
    (:import (java.io File InputStreamReader BufferedReader)))

(def google-code-url "http://code.google.com/p/clojure/source/browse/trunk/src/clj/")
(def google-java-code-url "http://code.google.com/p/clojure/source/browse/trunk/src/jvm/")
;;http://code.google.com/p/clojure/source/browse/trunk/src/jvm/clojure/lang/Cons.java?r=1334

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
             #(.. Runtime getRuntime (exec % %2 %3)));end comp
       (into-array ["svn" "info"])
       nil
       (File. "/home/hiredman/clojure/")))

(def clojurebot-rev (get-rev-number))

(defn get-file-and-ln [string]
      (let [a (meta (resolve (symbol string)))]
        [(:line a) (:file a)]))

(defn make-url [[line file]]
      (let [google (java.net.URLEncoder/encode (str google-code-url file "?r=" clojurebot-rev "#" line))
            url (str "http://tinyurl.com/api-create.php?url=" google)]
        (get-url url)))

(def make-url-cached (memoize make-url))

(def java-code-url (memoize (fn [url]
                                (get-url
                                  (str "http://tinyurl.com/api-create.php?url="
                                       (java.net.URLEncoder/encode url))))))

(defmethod responder ::code-lookup [bot msg]
  (let [message (extract-message bot msg)
        thing (second (.split #^String message " "))]
    (if (re-find #"[a-zA-z]+\.[a-zA-z]+\.[a-zA-z]+" thing)
      (let [[one two three] (.split thing "\\.")
            one (if (= one "c") "clojure" one)
            two (if (= two "l") "lang" two)
            thing (str one "." two "." three)
            thing2 (str one "/" two "/" three ".java")]  
      (send-out :notice bot (who msg) (str thing ": " (java-code-url (str google-java-code-url thing2 "?r=" clojurebot-rev)))))
      (send-out :notice bot (who msg) (try
                                        (str thing ": " (make-url-cached (get-file-and-ln thing)))
                                        (catch Exception e
                                               (befuddled)))))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^(def|source) " (extract-message bot msg)))) ::code-lookup)

;(count (re-find #"^(?:def|source) [^ ]+" "source foo bar"))
