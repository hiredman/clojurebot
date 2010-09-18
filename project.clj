(defproject
  clojurebot "0.1.0-SNAPSHOT"
  :repositories {"jboss" "http://repository.jboss.com/maven2/"}
  :description "An IRC bot written in Clojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.snuxoll/clojureql "1.0.0"]
                 [pircbot/pircbot "1.4.2"]
                 [org.danlarkin/clojure-json "1.1-SNAPSHOT"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [jivesoftware/smack "3.0.4"]
                 [org.clojars.hiredman/fnparse "2.2.4"]
                 [org.clojars.thnetos/opennlp "0.0.3"]
                 [jivesoftware/smackx "3.0.4"]]
  :dev-dependencies [[swank-clojure "1.2.1"]])
