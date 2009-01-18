(ns hiredman.pqueue
    (:refer-clojure :exclude [first conj])
    (:require [clojure.core :as cc]))

(defn first
      "returns the first item in priority queue"
      [x]
      (second (cc/first x)))

(defn conj
      "add item to priority queue nil is the same as 0
      [col [priority item]]"
      ([x f]
       (apply concat
              (interpose (list f)
                         (split-at
                           (count (take-while #(if (number? (cc/first f))
                                                 (< (if-let [t (cc/first %)]
                                                            t
                                                            0) (inc (cc/first f)))
                                                 (< (if-let [t (cc/first %)]
                                                            t
                                                            0) 1)) x))
                           x))))
      ([x f & y]
       (if y
         (recur (conj x f) (cc/first y) (rest y))
         (conj x f))))
