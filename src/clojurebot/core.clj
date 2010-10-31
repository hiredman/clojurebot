(ns clojurebot.core
  (:use [conduit.irc :only [a-irc irc-run]]
        [conduit.core]
        [clojurebot.conduit :only [a-indirect a-if a-cond null a-when]]
        [hiredman.clojurebot.factoids :only [factoid-lookup
                                             factoid-command?
                                             factoid-command-run
                                             factoid-lookup-no-fall-back]]
        [hiredman.clojurebot.ticket :only [ticket-search?
                                           search-tickets
                                           ticket-search?
                                           ticket-query?
                                           get-ticket-n
                                           contrib-ticket-query?
                                           get-contrib-ticket-n]]
        [hiredman.clojurebot.sb :only [eval-request?
                                       eval-message]]
        [hiredman.clojurebot.code-lookup :only [code-lookup?
                                                do-code-lookup]]
        [clojure.contrib.logging :only [info]]
        [clojurebot.seenx :only [log-user seenx-query?
                                 seen-user]]
        [clojurebot.delicious :only [contains-url?
                                     delicious]]
        [clojurebot.dice :only [roll-some-dice
                                dice-roll?]]
        [hiredman.clojurebot.google :only [google-search?
                                           google-search]]
        [swank.swank :only [start-repl]])
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

(defn question? [{:keys [message]}]
  (and message
       (> (count (.trim message)) 1)
       (= 1 (count (.split message " ")))
       (.endsWith message "?")))

(def-arr limit-length [x]
  (if (string? x)
    (let [out (apply str (take 200 x))]
      (if (> (count x) 200)
        (str out "...")
        out))
    x))

(def clojurebot-eval
  (a-comp (a-arr eval-message)
          (a-if vector?
                (a-arr
                 (fn [x]
                   (reduce #(str %1 %2 "\n") x)))
                pass-through)))

(def-arr reconnect [{:keys [server bot config]}]
  (letfn [(reconnect-fn []
            (try
              (when-not (.isConnected bot)
                (info "reconnecting")
                (.connect bot server))
              (catch Exception e
                (info "Failed to reconnect" e)
                (info "retrying in 60 seconds")
                (Thread/sleep (* 60 1000))
                reconnect-fn)))]
    (trampoline reconnect-fn)))

(def-arr rejoin [{:keys [message bot config]}]
  (doseq [c (:channels config)]
    (.joinChannel bot c)))

(def-arr nickserv-id [{:keys [bot config]}]
  (when (:nickserv-password config)
    (.sendMessage bot "nickserv" (str "identify " (:nickserv-password config)))))

(defn doc-lookup? [{:keys [message]}]
  (and message
       (.startsWith message "(doc ")))

(def math? (comp #(re-find #"^\([\+ / \- \*] [ 0-9]+\)" %)
                 str
                 :message))

(def-arr da-math [{:keys [message]}]
  (let [[op & num-strings] (re-seq #"[\+\/\*\-0-9]+" message)
        nums (map #(Integer/parseInt %) num-strings)]
    (let [out (-> (symbol "clojure.core" op)
                  (find-var)
                  (apply nums))]
      (if (> out 4)
        "*suffusion of yellow*"
        out))))

(def notice (a-arr (partial vector :notice)))

;; pipelines
(def addressed-pipeline
  (a-comp remove-nick-prefix
          (a-cond ticket-query?
                  (a-arr get-ticket-n)

                  contrib-ticket-query?
                  (a-arr get-contrib-ticket-n)

                  ticket-search?
                  (a-arr search-tickets)

                  code-lookup?
                  (a-comp (a-arr do-code-lookup)
                          notice)

                  google-search?
                  (a-arr google-search)

                  seenx-query?
                  (a-arr seen-user)

                  (comp factoid-command? :message)
                  (a-arr factoid-command-run)

                  (constantly true)
                  (a-arr factoid-lookup))))

(def pipeline
  (a-except
   (a-comp (a-all (a-arr log-user) ;enable "~seen foo" stuff

                  (a-when contains-url?
                          (a-arr delicious))

                  pass-through)

           (a-arr last) ;we only want the passed through value

           (a-cond doc-lookup?
                   (a-comp (a-arr #(update-in % [:message] (fn [x] (str "," x))))
                           (a-indirect #'pipeline))

                   math?
                   da-math

                   eval-request?
                   (a-comp (a-arr (fn [x] (info (str "evaling " (:message x))) x))
                           clojurebot-eval
                           limit-length)

                   dice-roll?
                   (a-arr roll-some-dice)

                   addressed?
                   addressed-pipeline

                   question?            ;ping? => PONG!
                   (a-comp (a-arr factoid-lookup-no-fall-back)
                           (a-if nil?
                                 null
                                 pass-through))

                   (fn [x] (= 1 (rand-int 200)))
                   addressed-pipeline

                   (comp (partial = :disconnect) :type)
                   reconnect

                   (comp (partial = :connect) :type)
                   (a-all rejoin
                          nickserv-id)

                   (constantly true)
                   (a-comp (a-arr #(dissoc % :config :bot))
                           null)))
   (a-arr (comp #(doto % .printStackTrace) first))))

;;/pipelines

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
    (when (:swank config)
      (future
        (start-repl (:swank config))))
    (letfn [(connect []
              (try
                (apply irc-run
                       (clojurebot config)
                       (:server config)
                       (:nick config)
                       (:threads config)
                       (:channels config))
                (catch Exception e
                  (info "Connection failed sleeping 1 minute" e)
                  (Thread/sleep (* 60 1000))
                  connect)))]
      (trampoline connect))))
