(ns hiredman.clojurebot.factoids
  (:require [clj-http.client :as http])
  (:import (java.util UUID)))

(create-ns 'clojurebot.core)
(intern 'clojurebot.core 'l)

(defn factoid-command? [{:keys [message config]}]
  (let [{:keys [body]} (http/get (:facts-service config)
                                 {:query-params {:op "factoid-command?"
                                                 :message message}})]
    (read-string body)))

(defn factoid-lookup [{:keys [message config bot] :as bag}]
  (let [id (str (UUID/randomUUID))]
    (swap! clojurebot.core/l assoc id bot)
    (try
      (let [{:keys [body]} (http/get (:facts-service config)
                                     {:query-params {:op "factoid-lookup"
                                                     :message message
                                                     :id (str id)}})]
        (read-string body))
      (finally
        (swap! clojurebot.core/l dissoc id)))))

(defn factoid-lookup-no-fall-back [{:keys [message config] :as bag}])

(defn factoid-command-run [{:keys [config message]}])
