(ns hiredman.pqueue
    (:refer-clojure :exclude [first conj])
    (:require [clojure.core :as cc]))

(defn first [x]
      (second (cc/first x)))

(defn conj
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
