(ns clojurebot.launch
  (:gen-class))

(defn -main [& [config-file]]
  (require 'clojurebot.core)
  ((resolve 'clojurebot.core/-main) config-file))
