(ns clojurebot.feed
  (:use [clojure.contrib.logging :only [info]])
  (:require [clojure.xml :as xml]
            [hiredman.utilities :as util]
            [clojure.set :as set]))

(defn stage1 [url]
  (filter #(= :entry (:tag %))
          (tree-seq map? (comp seq :content) (xml/parse url))))

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

(defonce ^{:private true} entry-cache (atom {}))

(defn atom-pull*
  ([url]
     (atom-pull* url url))
  ([url key]
     (info (format "atom-pull %s %s" url key))
     (let [seen-ids (set (get @entry-cache key))
           latest-entries (stage2 (stage1 url))
           new-ids (set/difference (set (map :id latest-entries))
                                   seen-ids)
           new-entries (filter (comp new-ids :id) latest-entries)]
       (swap! entry-cache
              update-in [key] (comp set #(take 100 %) #(into % new-ids) set))
       new-entries)))

(defn atom-pull
  ([url]
     (atom-pull url url))
  ([url key]
     (reduce #(str %1 %2 "\n") nil (take 5 (atom-pull* url key)))))
