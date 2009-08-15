(ns hiredman.clojurebot
  (:use (hiredman.clojurebot core svn))
  (:require (hiredman.clojurebot core dice sb seenx google delicious noise stock-quote
                                 factoids forget translate code-lookup javadoc ticket
                                 github)
            [hiredman.utilities :as util]
            [hiredman.twitter :as twitter]))

(set! *warn-on-reflection* true)

(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))

(def bot-attributes 
     {:nick "clojurebot"
      :network "irc.freenode.net"
      :channel "#clojure"
      :tweet true
      :sandbox-ns 'sandbox
      :store (agent {})
      :dict-dir "/home/hiredman/"}) ;; must include final slash

(def bot 
     (run-clojurebot mybot bot-attributes
       (load-dicts mybot)
       (load-store mybot)
       (watch-store mybot)
       (start-dump-thread mybot)
       (hiredman.clojurebot.github/start-github-watch mybot "#clojure")
       (println "Done loading!")))
