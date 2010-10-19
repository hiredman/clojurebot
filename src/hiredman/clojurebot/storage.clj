(ns hiredman.clojurebot.storage
  (:require [hiredman.triples :as trip]))

(defn db-name [{:keys [database]}]
  (let [name database]
    (when-not (.exists (java.io.File. name))
      (trip/create-store name))
    name))
