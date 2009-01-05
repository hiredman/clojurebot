; Sample setup file for clojurebot; best eval'd line-by-line

;(add-classpath "file:///Users/oranenj/koodi/VCS/clojurebot/")
(ns hiredman.clojurebot
  (:use (hiredman clojurebot-core clojurebot-svn)))

(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))

(def bot-attributes 
     {:nick "clojurebot"
      :network "irc.freenode.net"
      :channel "#clojure"
      :svn-url "http://clojure.googlecode.com/svn/"
      :sandbox-ns 'sandbox
      :dict-dir "/home/hiredman/"}) ;; must include final slash
     
(def bot 
     (run-clojurebot mybot bot-attributes
       (load-dicts mybot)
       (start-dump-thread mybot)
       (start-svn-notifier-thread mybot)
       (println "Done loading!")))
