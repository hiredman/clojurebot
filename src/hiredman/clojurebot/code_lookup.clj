;; DEPENDS clojure-json
(ns hiredman.clojurebot.code-lookup
    (:use (hiredman.clojurebot core))
    (:use (hiredman utilities))
    (:require [org.danlarkin.json :as json])
    (:import (java.io File InputStreamReader BufferedReader)))

(def google-code-url "http://code.google.com/p/clojure/source/browse/trunk/src/clj/")
(def google-java-code-url "http://code.google.com/p/clojure/source/browse/trunk/src/jvm/")
;;http://code.google.com/p/clojure/source/browse/trunk/src/jvm/clojure/lang/Cons.java?r=1334

(def contrib-url "http://github.com/richhickey/clojure-contrib/raw/gh-pages/api-index.json")

(def contrib (json/decode-from-str (get-url contrib-url)))

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

;;(def clojurebot-rev (get-rev-number))
(def clojurebot-rev 1)

(defn get-sha-number []
      ((comp; #(Integer/parseInt %)
             second
             #(.split % " ")
             first 
             line-seq
             #(BufferedReader. %)
             #(InputStreamReader. %)
             #(.getInputStream %)
             #(.. Runtime getRuntime (exec % %2 %3)));end comp
       (into-array ["git" "log" "-1"])
       nil
       (File. "/home/hiredman/clojure/")))

(def clojurebot-rev (get-sha-number))

(def foo "http://github.com/richhickey/clojure-contrib/blob/7ea70da82e42416864e2f97e3d314aced34af682/src/clojure/contrib/")
(def bar "http://code.google.com/p/clojure-contrib/source/browse/trunk/src/clojure/contrib/")

(defn google-code->github-url
  "transforms a googlecode source url into a github url"
  [url project rev]
  (-> url
    (.replaceAll "http://code.google.com/p/clojure/source/browse/trunk"
                 (str "http://github.com/richhickey/" project "/blob/" rev))
    (.replaceAll "\\?r=(.*)#(\\d+)" "#L$2")))


(defn get-file-and-ln [string]
      (let [a (meta (resolve (symbol string)))]
        [(:line a) (:file a)]))

(defn make-url [[line file]]
      (let [google (str google-code-url file "?r=" clojurebot-rev "#" line)
            google (google-code->github-url google "clojure" clojurebot-rev)]
        (tinyurl google)))
(def make-url-cached make-url)

(def java-code-url (memoize (fn [url]
                                (get-url
                                  (str "http://tinyurl.com/api-create.php?url="
                                       (java.net.URLEncoder/encode url))))))

(def java-code-url (fn [url] (tinyurl url)))

(defmulti lookup
  (fn [bot msg thing]
    (cond
      (re-find #"c\.l\.[a-zA-z]+" thing) :clojure-java
      (re-find #"[a-zA-z]+\.[a-zA-z]+\.[a-zA-z]+" thing) :java
      :else :clojure)))

(defmethod lookup :clojure-java [bot msg thing]
  (lookup bot msg (.replaceAll thing "c\\.l" "clojure.lang")))

(defmethod lookup :java [bot msg thing]
  (send-out :notice bot (who msg)
            (str thing ": "
                 (java-code-url
                   (google-code->github-url
                     (str google-java-code-url
                          (.replaceAll thing "\\." "/") ".java?r=" clojurebot-rev)
                     "clojure"
                     clojurebot-rev)))))

(defn contrib-lookup [thing]
  (map (comp #(get-url (str "http://tinyurl.com/api-create.php?url="
                       (java.net.URLEncoder/encode %)))
             ;;#(.replaceAll % "#(\\d)" "#L$1") ;;horrible patching for github
             ;;#(.replace % bar foo)
             :source-url)
       (filter #(= (:name %) thing) (:vars contrib))))

(defmethod lookup :clojure [bot msg thing]
  (let [[file line] (get-file-and-ln thing)]
    (if (or (nil? file) (nil? line))
      (if-let [results (seq (contrib-lookup thing))]
        (send-out :notice bot (who msg)
                  (reduce #(str % " " %2) (str thing ":") results))
        (send-out :msg bot (who msg) (befuddled)))
      (send-out :notice bot (who msg) (str thing ": " (make-url-cached [file line]))))))

(defresponder ::code-lookup 0
  (dfn (and (addressed? bot msg)
            (re-find #"^(def|source) " (extract-message bot msg))))
  (let [message (extract-message bot msg)
        thing (.replaceAll message "^(def|source) " "")]
    (lookup bot msg thing)))
