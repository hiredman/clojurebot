(defproject clojurebot "1.1.0-SNAPSHOT"
  :repositories {"sonatype-snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :description "An IRC bot written in Clojure"
  :exclusions [commons-logging]
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/tools.logging "0.2.6"]
                 [conduit-irc "2.0.1-SNAPSHOT"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [cheshire "5.0.2"]
                 [clj-http "0.7.2"
                  :exclude [commons-logging]]
                 [swank-clojure "1.3.2"]
                 [com.thelastcitadel/apropos "0.0.1"]
                 [ring "1.1.8"]
                 [compojure "1.1.5"]
                 ;; logging
                 [ch.qos.logback/logback-classic "1.0.9"]
                 [ch.qos.logback/logback-core "1.0.9"]
                 [org.slf4j/jcl-over-slf4j "1.7.2"]]
  :main clojurebot.launch
  :clean-non-project-classes false
  :min-lein-version "2.0.0")
