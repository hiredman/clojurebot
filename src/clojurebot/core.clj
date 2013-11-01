(ns clojurebot.core
  (:use [conduit.irc :only [irc-run a-irc *pircbot* pircbot]]
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
        [hiredman.clojurebot.code-lookup :only [code-lookup? do-code-lookup]]
        [clojurebot.eval :only [eval-request?]]
        [clojure.tools.logging :only [info]]
        [clojurebot.seenx :only [log-user seenx-query? seen-user]]
        [clojurebot.dice :only [roll-some-dice dice-roll?]]
        [hiredman.clojurebot.google :only [google-search? google-search]]
        [swank.swank :only [start-repl]]
        [clojurebot.epigrams :only [epigram-query? lookup-epigram]]
        [clojurebot.coreII :only [addressed? remove-nick-prefix question?
                                  limit-length clojurebot-eval reconnect
                                  rejoin nickserv-id doc-lookup? math? da-math
                                  notice target setup-crons]]
        [clojurebot.plugin :only [load-from]]
        [hiredman.clojurebot.simplyscala :only [scala-eval]]
        [compojure.core :only [defroutes GET]]
        [ring.adapter.jetty :only [run-jetty]]
        [com.thelastcitadel.apropos :only [apropos]])
  (:require [clojure.tools.logging :as log]))

(defn comic? [m]
  (when-let [v (resolve 'clojurebot.phil/comic?)]
    (v m)))

;; pipelines
;; addressed pipelines are run when a message has been determined to
;; have been addressed specificly at the bot
(def addressed-pipeline
  (a-comp remove-nick-prefix
          ;; stupid implemention looking for config defined
          ;; addressed-plugins ends up search through the list twice
          (a-all (a-arr
                  (fn [{:keys [config] :as a-map}]
                    (log/info "here")
                    ((comp boolean first filter)
                     (fn [[ns query action]]
                       (when-let [query (ns-resolve ns query)]
                         (query a-map)))
                     (:addressed-plugins config))))
                 pass-through)
          (a-select
           true (a-arr
                 (fn [{:keys [config] :as a-map}]
                   (log/info "here2")
                   (let [[ns query action] ((comp first filter)
                                            (fn [[ns query action]]
                                              (let [query (ns-resolve
                                                           ns query)]
                                                (log/info ns)
                                                (log/info "query" query)
                                                (when query
                                                  (query a-map))))
                                            (:addressed-plugins config))]
                     (log/info "action" action)
                     (@(ns-resolve ns action) a-map))))
           false (a-cond comic?
                         (a-arr (fn [m]
                                  (when-let [v (resolve 'clojurebot.phil/lookup-comic)]
                                    (v m))))
                         
                         (fn [{:keys [message] :as m}]
                           (when message
                             (.startsWith message "apropos ")))
                         (a-arr (fn [{:keys [message]}]
                                  (try
                                    (apropos (.replaceFirst message "apropos " ""))
                                    (catch Throwable _
                                      "dunno"))))
                         
                         ticket-query?
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

                         factoid-command?
                         (a-arr factoid-command-run)

                         (constantly true)
                         (a-arr factoid-lookup)


                         ))))

(def pipeline
  (a-except
   (a-comp
    (a-all
     (a-arr log-user) ;enable "~seen foo" stuff

     ;; run logging plugins
     (a-arr (fn [{:keys [config] :as msg}]
              (doseq [name (:logging-plugins config)]
                (try
                  ((resolve name) msg)
                  (catch Exception e
                    (log/error e "error running logging plugin"))))))
     pass-through)

    (a-arr last) ;we only want the passed through value

    (a-cond doc-lookup?
            (a-comp (a-arr
                     #(update-in % [:message] (fn [x] (str "," x))))
                    clojurebot-eval)

            math?
            da-math

            (fn [{:keys [message]}]
              (and message (.startsWith message ",scala")))
            (a-arr (fn [{:keys [message]}]
                     (scala-eval (.replaceFirst message ",scala" ""))))

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
            (a-all nickserv-id
                   rejoin)

            (comp (partial = :invite) :type)
            (a-comp (a-arr (fn [{:keys [bot channel config]}]
                             (when (:on-invite config)
                               (.joinChannel bot (last (.split channel " "))))))
                    null)

            (constantly true)
            (a-comp (a-arr #(dissoc % :config :bot))
                    null)
            ))
   (a-arr (comp #(log/error % "error") first))))

;;/pipelines

(defn clojurebot [config]
  (a-irc
   (:nick config)
   (a-comp
    (a-arr (fn [[type bag]]
             (assoc bag
               :type type
               :config config
               :time (System/currentTimeMillis))))
    (a-indirect #'pipeline))))

(defn set-properties! []
  (when (empty? (System/getProperty "java.security.policy"))
    (System/setProperty
     "java.security.policy"
     (str (.getResource (class clojurebot) "/example.policy"))))
  (System/setProperty "file.encoding" "utf8")
  (System/setProperty "swank.encoding" "utf8"))

(defn load-plugins [config]
  (load-from (:plugin-directory config)
             (concat (map first (:addressed-plugins config))
                     (->> (:cron config)
                          (map :task)
                          (map namespace)
                          (map symbol))
                     (map (comp symbol namespace) (:logging-plugins config)))))

(defn start-swank [config]
  (when (:swank config)
    (future
      (start-repl (:swank config)))))

(def l (atom {}))

(defroutes cb
  (GET "/befuddled" []
       {:status 200
        :headers {"Content-Type" "application/edn; charset=utf-8"}
        :body (let [x (hiredman.clojurebot.core/befuddled)]
                (log/debug "/befuddled ·" x)
                (pr-str x))})
  (GET "/ok" []
       {:status 200
        :headers {"Content-Type" "application/edn; charset=utf-8"}
        :body (pr-str (hiredman.clojurebot.core/ok))})
  (GET "/randomperson/:id" [id]
       {:status 200
        :headers {"Content-Type" "application/edn; charset=utf-8"}
        :body (pr-str (hiredman.clojurebot.core/random-person (get @l id)))})
  )

(defn -main [& [config-file]]
  (set-properties!)
  (let [config (read-string (slurp config-file))
        crons (delay (setup-crons config))]
    ;; load the namespaces for different kinds of plugins
    (when (:plugin-directory config)
      (load-plugins config))
    (binding [*ns* (create-ns 'sandbox)]
      (refer 'clojure.core))
    (start-swank config)
    (run-jetty #'cb
               {:port 3205
                :join? false})
    ;; for each server run irc-run
    (doseq [[server channels] (:irc config)]
      (let [out *out*
            config (assoc config
                     :server server
                     :channels channels)
            p (pircbot (:server config) (:nick config))]
        (dotimes [_ (:threads config)]
          (future
            (binding [*out* out
                      *pircbot* p]
              (try
                @crons
                (apply irc-run
                       (clojurebot config)
                       channels)
                (catch Exception e
                  (info e "Connection failed"))))))))
    @(promise)))
