(ns clojurebot.facts
  (:require [clojure.tools.logging :as log]
            [clojurebot.factoids :as f]))

(defmulti handler (fn [m] (get (:params m) "op")))

(defmethod handler "factoid-command?" [req]
  {:status 200
   :body (pr-str (boolean (f/factoid-command?
                           {:message (get (:params req) "message")})))})

(defmethod handler "factoid-lookup" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-lookup
                    {:message (get (:params req) "message")}))}))

(defmethod handler "factoid-lookup-no-fall-back" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-lookup-no-fall-back
                    {:message (get (:params req) "message")}))}))

(defmethod handler "factoid-command-run" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-command-run
                    {:message (get (:params req) "message")}))}))
