(ns hiredman.clojurebot.tao
  (:use [clojure.set :only [difference]])
  (:import (java.io ByteArrayInputStream)
           (clojure.lang PersistentQueue))
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.schedule :as sched]
            [hiredman.utilities :as util]))

(def thetao "https://twitter.com/statuses/user_timeline/200313960.rss")

(def seen-entries (atom #{}))

(defn get-entries []
  (->> thetao util/get-url .getBytes ByteArrayInputStream. clojure.xml/parse
       (tree-seq map? (comp seq :content))
       (filter #(= :item (:tag %)))
       (hash-map :content)
       (tree-seq map? (comp seq :content))
       (filter #(= :title (:tag %)))
       (map :content)
       (map first)
       (map #(.replaceAll % "^WonderTao: " ""))))

(def Q (ref PersistentQueue/EMPTY))

(defn enqueue [string]
  (dosync (alter Q conj string)))

(defn de-enqueue []
  (dosync
   (let [item (peek (ensure Q))]
     (alter Q pop)
     item)))

(defn go [bot channel]
  (sched/fixedrate
   {:task (fn []
            (let [entries (get-entries)
                  new-entries (difference (set entries) @seen-entries)
                  ordered-new-entries (filter new-entries entries)]
              (reset! seen-entries (set new-entries))
              (doseq [i ordered-new-entries]
                (enqueue i))))
    :start-delay 1
    :rate 60
    :unit (:minutes sched/unit)})
  (sched/fixedrate
   {:task (fn []
            (when-let [msg (de-enqueue)]
              (.sendMessage (:this bot) channel msg)))
    :start-delay 1
    :rate 5
    :unit (:minutes sched/unit)}))


