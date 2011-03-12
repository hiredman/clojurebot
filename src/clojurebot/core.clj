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
                                  notice target setup-crons]]
        [clojurebot.plugin :only [load-from]])
  (:gen-class))

;; pipelines
(def addressed-pipeline
  (a-comp remove-nick-prefix
          (a-all (a-arr
                  (fn [{:keys [config] :as a-map}]
                    ((comp boolean first filter)
                     (fn [[ns query action]]
                       (let [query (ns-resolve ns query)]
                         (query a-map)))
                     (:addressed-plugins config))))
                 pass-through)
          (a-select
           true (a-arr
                 (fn [{:keys [config] :as a-map}]
                   (let [[ns query action] ((comp first filter)
                                            (fn [[ns query action]]
                                              (let [query (ns-resolve
                                                           ns query)]
                                                (query a-map)))
                                            (:addressed-plugins config))]
                     (@(ns-resolve ns action) a-map))))
           false (a-cond ticket-query?
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

                         epigram-query?
                         (a-arr lookup-epigram)

                         (comp factoid-command? :message)
                         (a-arr factoid-command-run)

                         (constantly true)
                         (a-arr factoid-lookup)))))

(def pipeline
  (a-except
   (a-comp
    (a-all
     (a-arr log-user) ;enable "~seen foo" stuff
     (a-when contains-url?
             (a-arr delicious))
     (a-arr (fn [{:keys [config] :as msg}]
              (doseq [name (:logging-plugins config)]
                (try
                  ((resolve name) msg)
                  (catch Exception e
                    (.printStackTrace e))))))
     pass-through)

    (a-arr last) ;we only want the passed through value

    (a-cond doc-lookup?
            (a-comp (a-arr
                     #(update-in % [:message] (fn [x] (str "," x))))
                    clojurebot-eval)

            math?
            da-math

            eval-request?
            (a-comp (a-arr (fn [x]
                             (info (format "evaling %s for %s"
                                           (:message x)
                                           (:sender x)))
                             x))
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
    (a-arr (fn [[type bag]]
             (assoc bag
               :type type
               :config config
               :time (System/currentTimeMillis))))
    (a-indirect #'pipeline))))

(defn set-properties! []
  (System/setProperty "java.security.policy"
                      (str (.getResource (class clojurebot) "/example.policy")))
  (System/setProperty "file.encoding" "utf8")
  (System/setProperty "swank.encoding" "utf8"))

(defn -main [& [config-file]]
  (set-properties!)
  (let [config (read-string (slurp config-file))]
    (when (:plugin-directory config)
      (load-from (:plugin-directory config)
                 (concat (map first (:addressed-plugins config))
                         (->> (:cron config)
                              (map :task)
                              (map namespace)
                              (map symbol))
                         (map namespace (:logging-plugins config)))))
    (binding [*ns* (create-ns 'sandbox)]
      (refer 'clojure.core))
    (when (:swank config)
      (future
        (start-repl (:swank config))))
    (setup-crons config)
    (doseq [[server channels] (:irc config)]
      (let [out *out*
            config (assoc config
                     :server server
                     :channels channels)]
        (future
          (binding [*out* out]
            (letfn [(connect []
                      (try
                        (apply irc-run
                               (clojurebot config)
                               server
                               (:nick config)
                               (:threads config)
                               channels)
                        (catch Exception e
                          (info "Connection failed sleeping 1 minute" e)
                          (Thread/sleep (* 60 1000))
                          connect)))]
              (trampoline connect))))))
    @(promise)))
