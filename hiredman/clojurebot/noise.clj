(ns hiredman.clojurebot.noise
  (:require [hiredman.clojurebot.core :as core]
	    [hiredman.utilities :as util]))

(def lookup (fn [] (second (first (filter #(= (first %) :hiredman.clojurebot.factoids/lookup) (.getMethodTable core/responder))))))

(def wheel 500)

(core/defresponder2
  {:name ::noise
   :priority 100
   :dispatch (fn [& _] (= 1 (rand-int wheel)))
   :body (fn [bot msg]
           (when (not (= (:message msg) ""))
	     (binding [core/befuddled (constantly nil)]
	       ((lookup) bot msg))))})

#_(core/remove-dispatch-hook ::noise)

(core/defresponder2
  {:name ::question
   :priority  80
   :dispatch (fn [bot msg]
               (let [m (core/extract-message bot msg)
                     x (count(.split m " "))]
		 (and (> 2 x) (.endsWith m "?"))))
   :body (fn [bot msg]
           (when (not (= (:message msg) ""))
	     (binding [core/befuddled (constantly nil)]
	       ((lookup) bot msg))))})
