; Sample setup file for clojurebot; best eval'd line-by-line

;(add-classpath "file:///Users/oranenj/koodi/VCS/clojurebot/")
(ns hiredman.clojurebot
  (:use (hiredman clojurebot-core clojurebot-svn)))

(comment ;;;;;;; eval these in a repl
(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))

(def bot-attributes 
     {:nick "testclobot"
      :network "irc.freenode.net"
      :channel "#testbot-channel"
      :svn-url "http://clojure.googlecode.com/svn/"
      :sandbox-ns 'sandbox
      :dict-dir "/Users/oranenj/koodi/VCS/clojurebot/" ;; must include final slash
      :dict-basename "brain"}) ; defaults to same as :nick
     
(def testbot 
     (run-clojurebot mybot bot-attributes
       (load-dicts mybot)
       (start-dump-thread mybot)
       (start-svn-notifier-thread mybot)
       (println "Done loading!")))


(hiredman.sandbox/enable-security-manager)
) ;;;;;;;; end comment
