(ns hiredman.clojurebot.noise
    (:require [hiredman.clojurebot.core :as core]
              [hiredman.utilities :as util]))

(def lookup (second (first (filter #(= (first %) :hiredman.clojurebot.factoids/lookup) (.getMethodTable core/responder)))))

(def wheel 100)

(core/defresponder2
  {:name ::noise
   :priority 100
   :dispatch (fn [& _] (= 1 (rand-int wheel)))
   :body (fn [bot msg](when (not (= (:message msg) ""))
          (binding [core/befuddled (constantly nil)]
            (lookup bot msg))))})

;(core/remove-dispatch-hook ::noise)
