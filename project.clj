(defproject
  clojurebot "0.1.0-SNAPSHOT"
  :repositories {"jboss" "http://repository.jboss.com/maven2/"}
  :description "An IRC bot written in Clojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [conduit-irc "1.0.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :main clojurebot.core)
