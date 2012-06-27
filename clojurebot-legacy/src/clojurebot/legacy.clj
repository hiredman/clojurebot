(ns clojurebot.legacy
  (:require [clojure.java.jmx :as jmx])
  (:import (java.util.concurrent LinkedBlockingQueue))
  (:gen-class))

(defn -main [& args]
  (let [queue (LinkedBlockingQueue.)]
    (jmx/register-mbean
     (jmx/create-bean (atom {:inbox queue}))
     "clojurebot.legacy:name=Eval")
    (future
      (while true
        (let [{:keys [body reply-to tag]} (read-string (.take queue))]
          (when reply-to
            (let [reply-queue (jmx/read reply-to :inbox)]
              (.put reply-queue
                    (pr-str (try
                              {:good (eval body) :tag tag}
                              (catch Throwable t
                                {:bad (print-str t)
                                 :tag tag}))))))
          (try
            (eval body)
            (catch Throwable _)))))))
