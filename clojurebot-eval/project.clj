(defproject clojurebot-eval "1.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ring "1.1.8"]
                 [org.clojure/tools.logging "0.2.4"]
                 [sonian/carica "1.0.2"]]
  :plugins [[lein-ring "0.8.2"]]
  :ring {:handler clojurebot.eval/handler
         :init clojurebot.eval/init})
