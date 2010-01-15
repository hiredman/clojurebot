(ns hiredman.clojurebot
  (:use (hiredman.clojurebot core svn)
        [hiredman.factoid-server :only (factoid-server)])
  (:require (hiredman.clojurebot core dice sb seenx google delicious noise
                                 stock-quote factoids forget translate
                                 code-lookup javadoc ticket github xmpp
                                 simplyscala)
            [hiredman.clojurebot.xmpp :as xmpp]
            [hiredman.utilities :as util]
            [hiredman.twitter :as twitter]))

(set! *warn-on-reflection* true)

(import '(sun.misc Signal SignalHandler))
(defn install [sig handler]
    (Signal/handle (Signal. (name sig))
                   (proxy [SignalHandler] []
                     (handle [sig] (handler sig)))))

(let [properties (java.util.Properties.)
      p (fn [x] (.getProperty properties x))]
  (with-open [properties-file (-> Class
                                (.getResourceAsStream
                                  "/clojurebot.properties"))]
    (.load properties properties-file)
    (defonce #^{:private true} bot-attributes
      {:nick "clojurebot"
       :network "irc.freenode.net"
       :channel "#clojurebot"
       :tweet true
       :delicious [(p "delicious.user") (p "delicious.password")]
       :twitter [(p "twitter.user") (p "twitter.password")]
       :sandbox-ns 'sandbox
       :store (agent {})
       :factoid-server-port 4444
       ;:xmpp-connection (xmpp/connect (p "xmpp.jid") (p "xmpp.password"))
       :dict-dir (.concat (System/getProperty "user.dir") "/")}))) ;; must include final slash

;;set up sandbox namespace for evaling code
(binding [*ns* (create-ns (:sandbox-ns bot-attributes))]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))


(defonce #^{:private true} bot 
     (run-clojurebot mybot bot-attributes
       (load-dicts mybot)
       (load-store mybot)
       (watch-store mybot)
       (start-dump-thread mybot)
       ((fn [b]
          (install :TERM (fn [s] (.disconnect b) (System/exit 0)))) mybot)
       (xmpp/setup-listener mybot)
       (xmpp/connect-to-muc mybot "clojure@conference.thelastcitadel.com")
       (hiredman.clojurebot.github/start-github-watch mybot "#clojure")
       (factoid-server (:factoid-server-port mybot) mybot)
       (println "Done loading!")))


