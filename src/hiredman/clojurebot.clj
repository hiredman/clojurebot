(ns hiredman.clojurebot
  (:use (hiredman.clojurebot core svn)
        [hiredman.factoid-server :only (factoid-server)])
  (:require (hiredman.clojurebot core dice sb seenx google delicious noise
                                 stock-quote factoids translate
                                 code-lookup javadoc ticket github xmpp
                                 simplyscala clojars tao)
            [hiredman.clojurebot.xmpp :as xmpp]
            [hiredman.utilities :as util]
            [hiredman.twitter :as twitter])
  (:import (org.jibble.pircbot PircBot)))

(set! *warn-on-reflection* true)

(import '(sun.misc Signal SignalHandler))

(defn install [sig handler]
  (Signal/handle (Signal. (name sig))
                 (proxy [SignalHandler] []
                   (handle [sig] (handler sig)))))

(defn props []
  (let [properties (java.util.Properties.)]
    (with-open [properties-file (-> (fn []) class
                                    (.getResourceAsStream
                                     "/clojurebot.properties"))]
      (.load properties properties-file)
      (into {} properties))))

;;set up sandbox namespace for evaling code
(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date))
  (intern *ns* 'Thread (fn [& _] (throw (Exception. "DENIED"))))
  (intern *ns* 'java.lang.Thread (fn [& _] (throw (Exception. "DENIED")))))

(defn connect [bot]
  (.connect (:this bot) (:network bot))
  bot)

(defn join [bot]
  (doseq [channel (cons (:channel bot) (:channels bot))]
    (.joinChannel (:this bot) channel))
  bot)

(defn -main []
  (-> (let [p (props)]
        {:nick "clojurebot"
         :network "irc.freenode.net"
         :channel "#clojurebot"
         :tweet true
         :delicious [(p "delicious.user") (p "delicious.password")]
         :twitter [(p "twitter.user") (p "twitter.password")]
         :sandbox-ns 'sandbox
         :store (agent {})
         :factoid-server-port 4444
         :xmpp-connection (xmpp/connect (p "xmpp.jid") (p "xmpp.password"))
         ;; must include final slash
         :dict-dir (.concat (System/getProperty "user.dir") "/")})
      ((fn [attrs]
         (let [bot (pircbot attrs)]
           (wall-hack-method
            PircBot :setName [String] (:this bot) (:nick bot))
           bot)))
      connect
      join
      load-dicts
      load-store
      watch-store
      start-dump-thread
      xmpp/setup-listener
      xmpp/connect-to-muc
      ((fn [bot] (factoid-server (:factoid-server-port bot) bot)))
      hiredman.clojurebot.clojars/go
      (hiredman.clojurebot.tao/go "#clojurebot" 5)
      ((fn [bot]
         (intern *ns* (with-meta 'bot {:private true}) bot))))
  (println "Done loading!"))
