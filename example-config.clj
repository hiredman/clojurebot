{:nick "clojurebotIII"
 :irc {"irc.freenode.net" ["#clojurebot"]}
 :database "/tmp/bot.db"
 :threads 4
 :cron [{:task hiredman.clojurebot.clojars/go
         :rate 3600
         :targets [[:irc "clojurebotIII" "irc.freenode.net" "#clojurebot"]]}
        {:task clojurebot.github/commits
         :rate 3000
         :targets [[:irc "clojurebotIII" "irc.freenode.net" "#clojurebot"]]
         :arguments ["clojure/clojure"]}
        {:task clojurebot.github/commits
         :rate 3000
         :targets [[:irc "clojurebotIII" "irc.freenode.net" "#clojurebot"]]
         :arguments ["clojure/clojure-contrib"]}]
 :plugin-directory "/Users/hiredman/src/clojurebot/"
 :addressed-plugins [[clojurebot.indexing search? search]]
 :plugins #{[clojurebot.github-shas contains-sha? lookup-sha
             {:username "hiredman"
              :token "token"
              :project-username "sonian"
              :project "sa-safe"}]}
;; :logging-plugins #{clojurebot.indexing/index}
 :on-invite :join
 :clojure-jar "/Users/hiredman/src/clojure/clojure.jar"
 :swank 8888}
