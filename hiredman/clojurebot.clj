(ns hiredman.clojurebot
  (:use (hiredman.clojurebot core svn))
  (:require (hiredman.clojurebot core dice sb seenx google delicious noise stock-quote
                                 factoids forget translate javadoc ticket
                                 github)
            [hiredman.utilities :as util]))

(set! *warn-on-reflection* true)

(import '(sun.misc Signal SignalHandler))
(defn install [sig handler]
    (Signal/handle (Signal. (name sig))
                   (proxy [SignalHandler] []
                     (handle [sig] (handler sig)))))

(defonce bot-attributes 
     {:nick "cljbot"
      :network "irc.freenode.net"
      :channel "#clojurebot"
      :sandbox-ns 'sandbox
      :store (agent {})
      :dict-dir (.concat (System/getProperty "user.dir") "/")}) ;; must include final slash

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
       (println "Done loading!")))
