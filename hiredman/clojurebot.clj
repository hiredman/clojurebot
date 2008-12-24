(ns hiredman.clojurebot
  (:use (hiredman clojurebot-core clojurebot-svn)))

(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))

(run-clojurebot 
 {:nick "harblbot"
  :network "irc.freenode.net"
  :channel "#test-chousuke"
  :sandbox-ns 'sandbox}

; (load-dicts)
; (dump-thread)

 (start-svn-notifier "http://clojure.googlecode.com/svn/"))
