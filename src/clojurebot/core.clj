(ns clojurebot.core
  (:use [conduit.irc :only [a-irc irc-run]]
        [conduit.core]
        [clojurebot.conduit :only [a-indirect a-if a-cond]]
        [hiredman.clojurebot.factoids :only [factoid-lookup
                                             factoid-command?
                                             factoid-command-run]]
        [hiredman.clojurebot.ticket :only [ticket-search?
                                           search-tickets
                                           ticket-search?
                                           ticket-query?
                                           get-ticket-n
                                           contrib-ticket-query?
                                           get-contrib-ticket-n]]
        [clojure.contrib.logging :only [info]])
  (:gen-class))

(defn addressed?
  [{:keys [bot config message type] :as bag}]
  (and (or (= type :message)
           (= type :private-message))
       (or (re-find #"^~" message)
           (re-find (re-pattern (str "^" (.getNick bot) ":")) message)
           (re-find (re-pattern (str "^" (.getNick bot) ",")) message)
           (nil? (:channel bag)))))

(def-arr remove-nick-prefix [{:keys [bot] :as bag}]
  (update-in bag [:message]
             (fn [message]
               (let [prefixes [(str (.getNick bot) ":")
                               (str (.getNick bot) ",")
                               "~"]]
                 (.trim (reduce
                         #(.replaceAll %1 (str (re-pattern %2)) "")
                         message
                         prefixes))))))

(def-proc null [x]
  (info (str "Bit Bucket:" x))
  [])

(def addressed-pipeline
  (a-comp remove-nick-prefix
          (a-cond ticket-query?
                  (a-arr get-ticket-n)

                  contrib-ticket-query?
                  (a-arr get-contrib-ticket-n)

                  ticket-search?
                  (a-arr search-tickets)

                  (comp factoid-command? :message)
                  (a-arr factoid-command-run)

                  (constantly true)
                  (a-arr factoid-lookup))))

(defn question? [{:keys [message]}]
  (and message
       (= 1 (count (.split message " ")))
       (.endsWith message "?")))

(def pipeline
  (a-except
   (a-cond addressed?
           addressed-pipeline

           question? ;ping? => PONG!
           (a-arr factoid-lookup)

           (constantly true)
           null)
   (a-arr (comp #(doto % .printStackTrace) first))))

(defn clojurebot [config]
  (a-irc
   (:server config)
   (:nick config)
   (a-comp
    (a-arr (fn [[type bag]] (assoc bag :type type :config config)))
    (a-indirect #'pipeline))))

(defn -main [& [config-file]]
  (let [config (read-string (slurp config-file))]
    (dotimes [_ 4]
      (future
        (apply irc-run
               (clojurebot config)
               (:server config)
               (:nick config)
               (:channels config))))))
