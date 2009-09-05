(ns hiredman.clojurebot
  (:use (hiredman.clojurebot core svn))
  (:require (hiredman.clojurebot core dice sb seenx google delicious noise stock-quote
                                 factoids forget translate code-lookup javadoc ticket
                                 github xmpp)
            [hiredman.clojurebot.xmpp :as xmpp]
            [hiredman.utilities :as util]
            [hiredman.twitter :as twitter]))

(set! *warn-on-reflection* true)

(import '(sun.misc Signal SignalHandler))
(defn install [sig handler]
    (Signal/handle (Signal. (name sig))
                   (proxy [SignalHandler] []
                     (handle [sig] (handler sig)))))

(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))

(defonce bot-attributes 
     {:nick "clojurebot"
      :network "irc.freenode.net"
      :channel "#clojurebot"
      :tweet true
      :delicious ["username" "password"]
      :twitter ["username" "password"]
      :sandbox-ns 'sandbox
      :store (agent {})
      :xmpp-connection (xmpp/connect "jid@domain" "password")
      :dict-dir "/home/hiredman/"}) ;; must include final slash

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
       (println "Done loading!")))
