{:nick "clojurebotIII"
 :channels ["#clojurebot"]
 :server "irc.freenode.net"
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
 #_ :delicious #_ ["user" "password"]
 :on-invite :join}
