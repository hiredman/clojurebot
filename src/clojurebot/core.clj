(ns clojurebot.core
  (:use [conduit.irc :only [a-irc irc-run]]
        [conduit.core])
  (:gen-class))

(defn clojurebot [config]
  (a-irc
   (:server config)
   (:nick config)
   (a-comp (a-par pass-through (a-arr #(assoc % :config config)))
           (a-arr doall))))

(defn -main [& [config-file]]
  (let [config (read-string (slurp config-file))]
    (dotimes [_ 4]
      (apply irc-run
             (clojurebot config)
             (:server config)
             (:nick config)
             (:channels config)))))
