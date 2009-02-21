(ns hiredman.pqueue
    (:refer-clojure :exclude [first conj seq])
    (:require [clojure.core :as cc]))

;(in-ns 'hiredman.pqueue)

(defn seq
  "returns a lazy sequence of the items in the priority queue"
  [pq]
  (cc/seq (map second pq)))

(defn first
  "returns the first item in a priority queue"
  [pq]
  (cc/first (seq pq)))

(defn conj
  "adds items to the priority queue. items must be pairs of form
  [priority value]; nil as a priority is equal to 0."
  [pq & items]
  (sort-by #(if-let [x (cc/first %)]
              x
              0) (apply cc/conj pq items)))
