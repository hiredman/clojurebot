(ns clojurebot.facts
  (:require [clojure.tools.logging :as log]
            [clojurebot.factoids :as f]))

(defmulti handler (fn [m] (get (:params m) "op")))

(defmethod handler "factoid-command?" [req]
  {:status 200
   :body (boolean (f/factoid-command? (get (:params req) "message")))})

(defmethod handler "factoid-lookup" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-lookup (get (:params req) "message")))}))
