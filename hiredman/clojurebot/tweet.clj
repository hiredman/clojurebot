(ns hiredman.clojurebot.tweet
    (:require [hiredman.clojurebot.core :as core])
    (:user (hiredman horizon))
    (:import (java.util.concurrent TimeUnit/MINUTES)))

(def url "http://search.twitter.com/search.atom?q=")

(defn watch [term]
      (.scheduleAtFixedRate core/task-runner
                            #()
                            (long 0)
                            (long 60)
                            TimeUnit/MINUTES))
