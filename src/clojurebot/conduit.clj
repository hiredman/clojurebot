(ns clojurebot.conduit
  (:use [conduit.core]
        [clojure.tools.logging :only [info]]))

(defn a-indirect [proc-ref]
  (assoc pass-through
    :type :deref
    :reply (fn this-fn [x]
             (let [[new-value new-fn] ((:reply @proc-ref) x)]
               [new-value this-fn]))
    :no-reply (fn this-fn [x]
                (let [[new-value new-fn] ((:no-reply @proc-ref) x)]
                  [new-value this-fn]))
    :scatter-gather (fn this-fn [x]
                      (fn []
                        (let [[new-value new-fn]
                              (((:scatter-gather @proc-ref) x))]
                          [new-value this-fn])))))

(defn a-if [a b c]
  (a-comp (a-all (a-arr (comp boolean a))
                 pass-through)
          (a-select
           true b
           false c)
          pass-through))

(defn a-when [a b]
  (a-if a
        b
        pass-through))

(defn a-cond [predicate consequent & more]
  (if (seq more)
    (a-if predicate
          consequent
          (apply a-cond more))
    (a-if predicate
          consequent
          pass-through)))

(def-proc null [x]
  (info (str "Bit Bucket: " x))
  [])
