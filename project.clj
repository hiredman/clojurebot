(defproject clojurebot "0.3.2-SNAPSHOT"
  :repositories {"sonatype-snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :description "An IRC bot written in Clojure"
  :dependencies [[vespa-crabro "1.0.0-SNAPSHOT"]
                 [org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/java.jdbc "0.0.5"]
                 [org.clojure/tools.logging "0.1.2"]
                 [org.apache.derby/derby "10.8.1.2"]
                 [conduit-irc "2.0.1-SNAPSHOT"]
                 [clojure-opennlp "0.1.9"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [log4j "1.2.16"]
                 [org.danlarkin/clojure-json "1.1"]
                 [clj-http "0.1.1"]
                 [swank-clojure "1.3.2"]]
  :main clojurebot.core
  :clean-non-project-classes false)
