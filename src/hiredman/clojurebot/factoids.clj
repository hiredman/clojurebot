(ns hiredman.clojurebot.factoids
  (:require [clj-http.client :as http]
            [clojure.tools.logging :as log]
            [hiredman.utilities :as u])
  (:import (java.util UUID)))

(create-ns 'clojurebot.core)
(intern 'clojurebot.core 'l)

(defn factoid-command? [{:keys [message config sender]}]
  (u/with-breaker 10
    (let [{:keys [body]} (http/get (:facts-service config)
                                   {:query-params {:op "factoid-command?"
                                                   :message message
                                                   :sender sender}})]
      (read-string body))))

(defn factoid-lookup [{:keys [message config bot sender] :as bag}]
  (let [id (str (UUID/randomUUID))]
    (swap! clojurebot.core/l assoc id bot)
    (try
      (u/with-breaker 60
        (let [{:keys [body]}
              (http/get (:facts-service config)
                        {:query-params {:op "factoid-lookup"
                                        :message message
                                        :id (str id)
                                        :sender sender
                                        :befuddled-url "http://localhost:3205/befuddled"
                                        :ok-url "http://localhost:3205/ok"
                                        :randomperson-url
                                        (str "http://localhost:3205/randomperson/" id)}})]
          (read-string body)))
      (finally
        (swap! clojurebot.core/l dissoc id)))))

(defn factoid-lookup-no-fall-back [{:keys [message config bot sender] :as bag}]
  (try
    (let [id (str (UUID/randomUUID))]
      (swap! clojurebot.core/l assoc id bot)
      (try
        (u/with-breaker 60
          (let [{:keys [body]}
                (http/get (:facts-service config)
                          {:query-params {:op "factoid-lookup-no-fall-back"
                                          :message message
                                          :id (str id)
                                          :sender sender
                                          :befuddled-url "http://localhost:3205/befuddled"
                                          :ok-url "http://localhost:3205/ok"
                                          :randomperson-url
                                          (str "http://localhost:3205/randomperson/" id)}})]
            (read-string body)))
        (finally
          (swap! clojurebot.core/l dissoc id))))
    (catch Throwable t
      (log/info t)
      nil)))

(defn factoid-command-run [{:keys [config message bot sender]}]
  (let [id (str (UUID/randomUUID))]
    (swap! clojurebot.core/l assoc id bot)
    (try
      (u/with-breaker 60
        (let [{:keys [body]}
              (http/get (:facts-service config)
                        {:query-params {:op "factoid-command-run"
                                        :message message
                                        :id (str id)
                                        :sender sender
                                        :befuddled-url "http://localhost:3205/befuddled"
                                        :ok-url "http://localhost:3205/ok"
                                        :randomperson-url
                                        (str "http://localhost:3205/randomperson/" id)}})]
          (read-string body)))
      (finally
        (swap! clojurebot.core/l dissoc id)))))
