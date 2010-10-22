(defproject
  clojurebot "0.2.0-SNAPSHOT"
  :repositories {"jboss" "http://repository.jboss.com/maven2/"}
  :description "An IRC bot written in Clojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [conduit-irc "1.0.0-SNAPSHOT"]
                 [org.clojars.snuxoll/clojureql "1.0.0"]
                 [org.clojars.hiredman/fnparse "2.2.4"]
                 [org.clojars.thnetos/opennlp "0.0.3"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [log4j "1.2.16"]
                 [org.danlarkin/clojure-json "1.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :main clojurebot.core)
