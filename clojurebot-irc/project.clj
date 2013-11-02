(defproject clojurebot-irc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.2"]
                 [sonian/carica "1.0.2"]
                 [net.intensivesystems/conduit "0.7.0-SNAPSHOT"]
                 [org.clojure/tools.logging "0.2.6"]]
  :profiles {:dev {:exclusions [commons-logging]
                   :dependencies [[ch.qos.logback/logback-classic "1.0.9"]
                                  [ch.qos.logback/logback-core "1.0.9"]
                                  [org.slf4j/jcl-over-slf4j "1.7.2"]]}})
