(ns hiredman.pqueue
    (:refer-clojure :exclude [first conj seq])
    (:require [clojure.core :as cc]))

(defn seq
  "returns a lazy sequence of the items in the priority queue"
  [pq]
  (map second pq))

(defn first
  "returns the first item in a priority queue"
  [x]
  (cc/first (seq pq)))

(defn conj
  "adds items to the priority queue. items must be pairs of form
  [priority value]; nil as a priority is equal to 0."
  [pq & items]
  (sort-by #(if-let [x (cc/first %)]
              x
              0) (apply cc/conj pq items)))
