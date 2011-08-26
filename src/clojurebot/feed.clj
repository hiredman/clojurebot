(ns clojurebot.feed
  (:use [clojure.tools.logging :only [info]])
  (:require [clojure.xml :as xml]
            [hiredman.utilities :as util]
            [clojure.set :as set]
            [clj-http.client :as http]))

(defn stage1 [url]
  (filter #(= :entry (:tag %))
          (tree-seq map? (comp seq :content) (xml/parse url))))

(defn stage1* [url & [username password]]
  (->> (http/get url {:basic-auth [username password]})
       :body
       .getBytes
       (java.io.ByteArrayInputStream.)
       (xml/parse)
       (tree-seq map? (comp seq :content))))

(defn find-tag [tag top]
  (->> top :content (filter #(= tag (:tag %))) first))

(defn id [entry]
  (->> entry (find-tag :id) :content first))

(defn link [entry]
  (->> entry (find-tag :link) :attrs :href))

(defn title [entry]
  (->> entry (find-tag :title) :content first))

(defn author [entry]
  (->> entry (find-tag :author) (find-tag :name) :content first))

(defn updated [entry]
  (->> entry (find-tag :updated) :content first))

(defn entry->map [entry]
  {:id (id entry)
   :link (util/tinyurl (link entry))
   :title (title entry)
   :author (author entry)
   :date (updated entry)})

(defn stage2 [entries]
  (map entry->map entries))

(defn rss-entries [url & [username password]]
  (->> (stage1* url username password)
       (filter #(= :entry (:tag %)))
       (map entry->map)))

(def last-seen 500)

(defonce ^{:private true} entry-cache (atom {}))

(defn atom-pull*
  ([url]
     (atom-pull* url url))
  ([url key]
     (info (format "atom-pull %s %s" url key))
     (let [ids (get @entry-cache key)
           seen-ids (set ids)
           latest-entries (stage2 (stage1 url))
           new-ids (set/difference (set (map :id latest-entries))
                                   seen-ids)
           new-entries (reverse (filter (comp new-ids :id) latest-entries))]
       (swap! entry-cache update-in [key]
              (comp set #(take last-seen %) #(into % new-ids) set))
       (println new-entries)
       new-entries)))

(defn atom-pull
  ([url]
     (atom-pull url url))
  ([url key]
     (reduce #(str %1 %2 "\n") nil (take 5 (atom-pull* url key)))))

(defn rss-pull* [url & [username password]]
  (info (format "rss-pull %s" url))
  (let [ids (get @entry-cache url)
        seen-ids (set ids)
        latest-entries (rss-entries url username password)
        new-ids (set/difference (set (map :id latest-entries))
                                seen-ids)
        new-entries (reverse (filter (comp new-ids :id) latest-entries))]
    (swap! entry-cache update-in [url]
           (comp set #(take last-seen %) #(into % new-ids) set))
    new-entries))

(defn rss-pull [url & [username password]]
  (reduce #(str %1 %2 "\n") nil (take 5 (rss-pull* url username password))))

