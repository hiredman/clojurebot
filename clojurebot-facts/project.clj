(defproject clojurebot-facts "1.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ring "1.1.8"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.apache.derby/derby "10.8.1.2"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [factual/fnparse "2.3.0"]
                 [postgresql/postgresql "9.1-901.jdbc4"]
                 [clj-http "0.6.4" :exclude [cheshire]]
                 [clojure-opennlp "0.1.9"]
                 [org.clojure/core.logic "0.8.3"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 ;; logging
                 [ch.qos.logback/logback-classic "1.0.9"]
                 [ch.qos.logback/logback-core "1.0.9"]
                 [org.slf4j/jcl-over-slf4j "1.7.2"]
                 [com.thelastcitadel/m29 "0.1.0"]]
  :plugins [[lein-ring "0.8.2"]]
  :ring {:handler clojurebot.facts/handler
         :init clojurebot.facts/init})
