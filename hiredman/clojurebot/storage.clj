(ns hiredman.clojurebot.storage
  (:require [hiredman.triples :as trip])

(defn db-name [bot]
  (let [name (str (:dict-dir bot) (:nick bot) ".db")]
    (when-not (.exists (java.io.File. name))
      (trip/create-store name))
    name))
