(defproject clojurebot-facts "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [ring "1.1.8"]
                 [org.clojure/tools.logging "0.2.4"]
                 [sonian/carica "1.0.2"]
                 [org.apache.derby/derby "10.8.1.2"]
                 [org.clojure/java.jdbc "0.0.5"]
                 [org.clojars.hiredman/fnparse "2.2.4"]]
  :plugins [[lein-ring "0.8.2"]]
  :ring {:handler clojurebot.facts/handler})
