(ns hiredman.clojurebot.tao
  (:use [clojure.set :only [difference]])
  (:import (java.io ByteArrayInputStream)
           (clojure.lang PersistentQueue))
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.schedule :as sched]
            [hiredman.utilities :as util]))

(def thetao "https://twitter.com/statuses/user_timeline/200313960.rss")

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

(defn go [bot channel n]
  (let [seen-entries (atom #{})
        Q (ref PersistentQueue/EMPTY)]
    (letfn [(enqueue [string]
              (dosync (alter Q conj string)))
            (de-enqueue []
              (dosync
               (let [item (peek (ensure Q))]
                 (alter Q pop)
                 item)))]
      (sched/fixedrate
       {:task (fn []
                (let [entries (get-entries)
                      new-entries (difference (set entries) @seen-entries)
                      ordered-new-entries (reverse (filter new-entries entries))]
                  (reset! seen-entries (set new-entries))
                  (doseq [i ordered-new-entries]
                    (enqueue i))))
        :start-delay 1
        :rate 60
        :unit (:minutes sched/unit)})
      (sched/fixedrate
       {:task (fn []
                (Thread/sleep (* (rand-int 3) 1000 60 (rand-int 5)))
                (when-let [msg (de-enqueue)]
                  (.sendMessage (:this bot) channel msg)))
        :start-delay 1
        :rate n
        :unit (:minutes sched/unit)}))))


