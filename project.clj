(defproject clojurebot "0.3.2-SNAPSHOT"
  :repositories {"sonatype-snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :description "An IRC bot written in Clojure"
  :dependencies [[vespa-crabro "1.0.0-SNAPSHOT"]
                 [org.clojure/clojure "1.5.0-master-SNAPSHOT"]
                 [org.clojure/java.jdbc "0.0.5"]
                 [org.clojure/tools.logging "0.1.2"]
                 [org.apache.derby/derby "10.8.1.2"]
                 [conduit-irc "2.0.1-SNAPSHOT"]
                 [clojure-opennlp "0.1.9"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [log4j "1.2.16"]
                 [cheshire "4.0.0"]
                 [clj-http "0.4.1"]
                 [swank-clojure "1.3.2"]
                 [clj-wallhack "1.0"]
                 [polycosm "0.0.2-SNAPSHOT"]
                 [org.clojure/java.jmx "0.2.0"]]
  :main clojurebot.boot
  :clean-non-project-classes false)
