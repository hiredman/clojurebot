(defproject clojurebot "0.3.3-SNAPSHOT"
  :repositories {"sonatype-snapshots"
                 "https://oss.sonatype.org/content/repositories/snapshots/"}
  :description "An IRC bot written in Clojure"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/tools.logging "0.1.2"]
                 [conduit-irc "2.0.1-SNAPSHOT"]
                 #_[org.clojars.thnetos/opennlp "0.0.3"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [log4j "1.2.16"]
                 [org.danlarkin/clojure-json "1.1"]
                 [clj-http "0.1.1"]
                 [swank-clojure "1.3.2"]
                 [com.thelastcitadel/apropos "0.0.1"]
                 #_[conduit-xmpp "1.0.0-SNAPSHOT"]
                 [ring "1.1.8"]
                 [compojure "1.1.5"]]
  :main clojurebot.core
  :clean-non-project-classes false
  :min-lein-version "2.0.0")
