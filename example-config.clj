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
 :plugin-directory "/Users/hiredman/src/clojurebot/checkouts/"
 #_:addressed-plugins [[clojurebot.hudson build? do-a-build]]
 #_ :hudson "http://example.com/"
 #_ :delicious #_ ["user" "password"]
 :on-invite :join}
