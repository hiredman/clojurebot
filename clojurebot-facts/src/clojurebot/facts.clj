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

(def senders (atom {}))

(defonce fut (delay
              (future
                (while true
                  (Thread/sleep (* 1000 60 5))
                  (reset! senders {})))))

(defn wrap-record-sender [f]
  (fn [req]
    (swap! senders update-in [(get (:params req) "sender")] (fnil inc 0))
    (f req)))

(defn wrap-throttle-sender[f]
  (fn [req]
    (when (> (or (get @senders (get (:params req) "sender")) 0)
             10)
      (assert nil))
    (f req)))

(defn wrap-log-request [f]
  (fn [req]
    (log/info req)
    (f req)))

(defn wrap-edn-response [f]
  (fn [req]
    (let [r (f req)]
      (update-in r [:headers] assoc "Content-Type" "application/edn; charset=utf-8"))))

(def handler (-> #'handler*
                 wrap-record-sender
                 wrap-throttle-sender
                 ((fn [f]
                    (fn [req]
                      (if (contains? #{"logic_prog"
                                       "ddellacosta"
                                       "bitemyapp"
                                       "arrdem"}
                                     (get (:params req) "sender"))
                        (assert nil)
                        (f req)))))
                 wrap-log-request
                 wrap-edn-response
                 mw/wrap-params))

(defn init []
  @fut
  (nrepl/start-server :port 5678))
