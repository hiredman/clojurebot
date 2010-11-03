{:nick "clojurebotIII"
 :channels ["#clojurebot"]
 :server "irc.freenode.net"
 :database "/tmp/bot.db"
 :threads 4
 :cron [{:task hiredman.clojurebot.clojars/go
         :rate 3600
         :targets [[:irc "clojurebotIII" "irc.freenode.net" "#clojurebot"]]}]
 #_ :delicious #_ ["user" "password"]}


