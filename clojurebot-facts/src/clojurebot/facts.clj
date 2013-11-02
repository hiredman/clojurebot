(ns clojurebot.facts
  (:require [clojure.tools.logging :as log]
            [clojurebot.factoids :as f]
            [ring.middleware.params :as mw]
            [clojure.tools.nrepl.server :as nrepl]))

(defmulti handler* (fn [m] (get (:params m) "op")))

(defmethod handler* "factoid-command?" [req]
  {:status 200
   :body (pr-str (boolean (f/factoid-command?
                           {:message (get (:params req) "message")
                            :sender (get (:params req) "sender")})))})

(defmethod handler* "factoid-lookup" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-lookup
                    {:message (get (:params req) "message")
                     :sender (get (:params req) "sender")}))}))

(defmethod handler* "factoid-lookup-no-fall-back" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-lookup-no-fall-back
                    {:message (get (:params req) "message")
                     :sender (get (:params req) "sender")}))}))

(defmethod handler* "factoid-command-run" [req]
  (binding [f/*id* (get (:params req) "id")]
    {:status 200
     :body (pr-str (f/factoid-command-run
                    {:message (get (:params req) "message")
                     :sender (get (:params req) "sender")}))}))


(def handler (-> #'handler*
                 ((fn [f]
                    (fn [req]
                      (log/info req)
                      (f req))))
                 ((fn [f]
                    (fn [req]
                      (let [r (f req)]
                        (update-in r [:headers] assoc "Content-Type" "application/edn; charset=utf-8")))))
                 mw/wrap-params))

(defn init []
  (nrepl/start-server :port 5678))
