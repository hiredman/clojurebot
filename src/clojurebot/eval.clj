(ns clojurebot.eval
  (:require [clj-http.client :as http]
            [hiredman.clojurebot.core :as cc]
            [clojure.tools.logging :as log]
            [hiredman.utilities :as u]))

(defn naughty-forms? [strang]
  (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot"
             "java.lang.Thread."}]
    (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

(defn eval-request? [{:keys [message]}]
  (and message (re-find #"^," (.trim message))))

(defn eval-message [{:keys [message sender config] :as bag}]
  (if (and (not (naughty-forms? message))
           (not= sender "itistoday")
           (not= sender "Lajla")
           (not= sender "Lajjla"))
    (try
        (u/with-breaker 20
          (let [{:keys [body] :as result} (http/get (config :evaluator)
                                                    {:query-params {:expression (.replaceAll message "^," "")
                                                                    :befuddled (pr-str ::befuddled)}
                                                     ;; 10 seconds
                                                     :socket-timeout (* 1000 10)
                                                     :conn-timeout (* 1000 10)})
                {:keys [stdout stderr result]} (read-string body)]
            (if (or (= result (pr-str ::befuddled))
                    (= result (prn-str ::befuddled)))
              (cc/befuddled)
              [stdout stderr result])))
        (catch Throwable t
          (log/info t "eval request failed")
          "eval service is offline"))
    (str sender ": " (cc/befuddled))))
