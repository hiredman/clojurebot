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
        [hiredman.clojurebot.sb :only [eval-request?
                                       eval-message]]
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

(defn question? [{:keys [message]}]
  (and message
       (= 1 (count (.split message " ")))
       (.endsWith message "?")))

(def-arr limit-length [x]
  (let [y (print-str x)
        out (apply str (take 200 y))]
    (if (> (count y) 200)
      (str out "...")
      out)))

(def clojurebot-eval
  (a-comp (a-arr eval-message)
          (a-if vector?
                (a-arr
                 (fn [x]
                   (reduce #(str %1 %2 "\n") x)))
                pass-through)))

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

(def pipeline
  (a-except
   (a-comp (a-cond (fn [{:keys [message]}]
                     (and message
                          (or (.startsWith message ",(doc ")
                              (.startsWith message "(doc "))))
                   (a-comp (a-arr (fn [_] (info "doc request")))
                           null)
            
                   eval-request?
                   clojurebot-eval

                   addressed?
                   addressed-pipeline

                   question?            ;ping? => PONG!
                   (a-arr factoid-lookup)

                   (constantly true)
                   null)
           limit-length)
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
    (binding [*ns* (create-ns 'sandbox)]
      (refer 'clojure.core))
    (dotimes [_ 4]
      (future
        (apply irc-run
               (clojurebot config)
               (:server config)
               (:nick config)
               (:channels config))))))
