(ns clojurebot.core
  (:use [conduit.irc :only [a-irc irc-run]]
        [conduit.core]
        [hiredman.clojurebot.factoids :only [factoid-lookup
                                             factoid-command?
                                             factoid-command-run]])
  (:gen-class))

(defn addressed?
  [{:keys [bot config message type] :as bag}]
  (and (or (= type :message)
           (= type :private-message))
       (or (re-find #"^~" message)
           (re-find (re-pattern (str "^" (.getNick bot) ":")) message)
           (re-find (re-pattern (str "^" (.getNick bot) ",")) message)
           (nil? (:channel bag)))))

(defn a-if [a b c]
  (a-comp (a-all (a-arr (comp boolean a))
                 pass-through)
          (a-select
           true b
           false c)))

(def-arr remove-nick-prefix [{:keys [bot] :as bag}]
  (update-in bag [:message]
             (fn [message]
               (let [prefixes [(str (.getNick bot) ":")
                               (str (.getNick bot) ",")
                               "~"]]
                 (reduce
                  #(.replaceAll %1 %2 "")
                  message
                  prefixes)))))

(defn a-cond [predicate consequent & more]
  (if (seq more)
    (a-if predicate
          consequent
          (apply a-cond more))
    (a-if predicate
          consequent
          pass-through)))

(defn clojurebot [config]
  (a-irc
   (:server config)
   (:nick config)
   (a-comp
    (a-arr (fn [[type bag]] (assoc bag :type type :config config)))
    (a-if addressed?
          (a-comp remove-nick-prefix
                  (a-cond
                   (comp factoid-command? :message)
                   factoid-command-run
                   
                   (constantly true)
                   (a-arr factoid-lookup)))
          pass-through))))

(defn -main [& [config-file]]
  (let [config (read-string (slurp config-file))]
    (dotimes [_ 4]
      (apply irc-run
             (clojurebot config)
             (:server config)
             (:nick config)
             (:channels config)))))
