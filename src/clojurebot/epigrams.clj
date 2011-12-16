(ns clojurebot.epigrams)

(defn startparse-tagsoup [s ch]
  (let [p (org.ccil.cowan.tagsoup.Parser.)]
    (.setContentHandler p ch)
    (.parse p s)))

(defn zip-soup [url]
  (clojure.zip/xml-zip (clojure.xml/parse url startparse-tagsoup)))

(def url "http://www.cs.yale.edu/quotes.html")

(defn find-tag [tag top]
  (->> top :content (filter #(= tag (:tag %))) first))

(defonce eps
  (->> url
       zip-soup
       first
       (find-tag :body)
       :content
       second
       :content
       (filter #(= :p (:tag %)))
       (map :content)
       (map first)
       (remove nil?)
       (map #(.replaceAll % "\n" " "))
       (map #(.trim %))
       vec
       delay))

(defn epigram-query? [{:keys [message]}]
  (and message
       (re-find #"^#\d+" message)
       (> 121 (Integer/parseInt (apply str (rest message))) 0)))

(defn lookup-epigram [{:keys [message]}]
  (let [n (dec (Integer/parseInt (apply str (rest message))))]
    (str (nth @eps n)
         " -- Alan J. Perlis")))
