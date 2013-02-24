(defproject clojurebot-irc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"jboss4" "http://repository.jboss.org/nexus/content/groups/public-jboss/"
                 "hornetq" "http://hornetq.s3.amazonaws.com/"}
  :exclusions [commons-logging]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [pircbot/pircbot "1.4.2"]
                 [vespa-crabro "0.2.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [ch.qos.logback/logback-classic "1.0.9"]
                 [ch.qos.logback/logback-core "1.0.9"]]
  :main clojurebot.irc
  :aot #{clojurebot.irc})
