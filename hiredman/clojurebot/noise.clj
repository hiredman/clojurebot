(ns hiredman.clojurebot.noise
    (:require [hiredman.clojurebot.core :as core]
              [hiredman.utilities :as util]))

(def lookup (second (first (filter #(= (first %) :hiredman.clojurebot.core/lookup) (.getMethodTable core/responder)))))

(core/defresponder ::noise 100
  (fn [& _] (= 1 (rand-int 100))),
  (when (not (= (:message msg) ""))
    (binding [core/befuddled (constantly nil)]
      (lookup bot msg))))

;(core/remove-dispatch-hook ::noise)
