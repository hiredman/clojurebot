(ns clojurebot.conduit
  (:use [conduit.core]))

(defn a-deref [proc-ref]
  (assoc pass-through
    :type :deref
    :reply (fn [x] ((:reply @proc-ref) x))
    :no-reply (fn [x] ((:reply @proc-ref) x))
    :scatter-gather (fn [x] ((:scatter-gather @proc-ref) x))))
