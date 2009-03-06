; Sample setup file for clojurebot; best eval'd line-by-line

;(add-classpath "file:///Users/oranenj/koodi/VCS/clojurebot/")
(ns hiredman.clojurebot
  ;(:use (hiredman.clojurebot core)))
  (:use (hiredman.clojurebot core svn))
  (:require (hiredman.clojurebot core svn dice sb seenx google forget translate code-lookup javadoc tweet)))

(set! *warn-on-reflection* true)

(binding [*ns* (create-ns 'sandbox)]
  (clojure.core/refer 'clojure.core)
  (import '(java.util Date)))

(def bot-attributes 
     {:nick "clojurebot"
      :network "irc.freenode.net"
      :channel "#clojurebot"
      :svn-url "http://clojure.googlecode.com/svn/" ;depricated?
      :sandbox-ns 'sandbox
      :dict-dir "/home/hiredman/"}) ;; must include final slash

(swap! default-repo (constantly "http://clojure.googlecode.com/svn/"))
     
(def bot 
     (run-clojurebot mybot bot-attributes
       (load-dicts mybot)
       (start-dump-thread mybot)
       (start-svn-watcher mybot :clojure "http://clojure.googlecode.com/svn/" clojure-channel-helper-callback)
       (start-svn-watcher mybot :contrib "http://clojure-contrib.googlecode.com/svn/"
                          (fn [bot revs]
                                (is! "latest contrib" (first (last (sort-by first revs))))
                                (doseq [r revs]
                                       (doseq [c (.getChannels (:this bot))]
                                              (send-out :notice bot c (str "Contrib: r"(first r) " " (second r)))))))
       ;(hiredman.clojurebot.tweet/watch mybot "clojure" "#clojure")
       (println "Done loading!")))
