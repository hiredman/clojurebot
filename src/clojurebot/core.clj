(ns clojurebot.core
  (:use [conduit.irc :only [irc-run a-irc]]
        [conduit.core]
        [clojurebot.conduit :only [a-indirect a-if a-cond null a-when]]
        [hiredman.clojurebot.factoids :only [factoid-lookup factoid-command?
                                             factoid-command-run
                                             factoid-lookup-no-fall-back]]
        [hiredman.clojurebot.ticket :only [ticket-search? search-tickets
                                           ticket-search? ticket-query?
                                           get-ticket-n contrib-ticket-query?
                                           get-contrib-ticket-n]]
        [hiredman.clojurebot.code-lookup :only [code-lookup? do-code-lookup]]
        [hiredman.clojurebot.sb :only [eval-request?]]
        [clojure.contrib.logging :only [info]]
        [clojurebot.seenx :only [log-user seenx-query? seen-user]]
        [clojurebot.delicious :only [contains-url? delicious]]
        [clojurebot.dice :only [roll-some-dice dice-roll?]]
        [hiredman.clojurebot.google :only [google-search? google-search]]
        [swank.swank :only [start-repl]]
        [clojurebot.epigrams :only [epigram-query? lookup-epigram]]
        [clojurebot.coreII :only [addressed? remove-nick-prefix question?
                                  limit-length clojurebot-eval reconnect
                                  rejoin nickserv-id doc-lookup? math? da-math
                                  notice target setup-crons]])
  (:gen-class))

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

                  clojurebot.epigrams/epigram-query?
                  (a-arr clojurebot.epigrams/lookup-epigram)

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
                           clojurebot-eval)

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

                   #(and (= 1 (rand-int 1000))
                         (= (:type %) :message))
                   addressed-pipeline

                   (comp (partial = :disconnect) :type)
                   reconnect

                   (comp (partial = :connect) :type)
                   (a-all rejoin
                          nickserv-id)

                   (comp (partial = :invite) :type)
                   (a-comp (a-arr (fn [{:keys [bot channel config]}]
                                    (when (:on-invite config)
                                      (.joinChannel bot channel))))
                           null)
                   
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
    (setup-crons config)
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
