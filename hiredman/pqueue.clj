(ns hiredman.pqueue
    (:refer-clojure :exclude [first conj seq pop peek empty])
    (:require [clojure.core :as cc]))

(def empty clojure.lang.PersistentQueue/EMPTY)

(defn seq
  "returns a lazy sequence of the items in the priority queue"
  [pq]
  (cc/seq (map second pq)))

(defn peek [pq]
  (cc/first (cc/peek pq)))

(defn pop [pq]
  (pop pq))

(defn first
  "returns the first item in a priority queue"
  [pq]
  (peek pq))

(defn conj
  [que & values]
  (let [entries (cc/seq (apply hash-map values))
        s (concat entries (cc/seq que))]
    (into empty (sort-by #(if-let [x (cc/first %)] x 0) s))))
