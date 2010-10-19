(ns clojurebot.conduit
  (:use [conduit.core]))

(defn a-indirect [proc-ref]
  (assoc pass-through
    :type :deref
    :reply (fn this-fn [x]
             (let [[new-value new-fn] ((:reply @proc-ref) x)]
               [new-value this-fn]))
    :no-reply (fn this-fn [x]
                (let [[new-value new-fn] ((:reply @proc-ref) x)]
                  [new-value this-fn]))
    :scatter-gather (fn this-fn [x]
                      (fn []
                        (let [[new-value new-fn] (((:scatter-gather @proc-ref) x))]
                          [new-value this-fn])))))
