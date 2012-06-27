(ns clojurebot.boot
  (:require [vespa.crabro :as vc]
            [vespa.protocols :as vp]
            [clojurebot.boot.legacy :as legacy])
  (:gen-class))

(defn context []
  (let [c (atom ())]
    (reify
      clojure.lang.IDeref
      (deref [_]
        c)
      java.io.Closeable
      (close [_]
        (doseq [thing @c]
          (try
            (.close thing)
            (catch Exception e
              (prn e))))))))

(defn load-transport [{:keys [type]}]
  (require (symbol (str "transports." (name type)))))

(defn boot-transport [{:keys [type] :as con} in out]
  ((resolve (symbol (str "transports." (name type)) "init"))
   con in out))

(defonce server (delay (vc/create-server)))

(defn -main [& [config-file]]
  (let [config (read-string (slurp config-file))
        in-queue (:in-queue config "clojurebot.in")
        out-queue (:out-queue config "clojurebot.out")
        exit-queue (:exit-queue config "clojurebot.exit")]
    (with-open [context (context)]
      (swap! @context conj @server)
      (legacy/boot)
      (doseq [con (:connections config)
              :let [mb (vc/message-bus)]]
        (swap! @context conj mb)
        (vp/declare-broadcast mb in-queue)
        (vp/declare-broadcast mb out-queue)
        (load-transport con)
        (boot-transport con
                        (partial vc/send-to mb in-queue)
                        (partial vp/receive-from mb out-queue)))
      (future
        (try
          (with-open [mb (vc/message-bus)]
            (while true
              (vp/receive-from mb in-queue prn)))
          (catch Exception e
            (.printStackTrace e))))
      (with-open [mb (vc/message-bus)]
        (vp/receive-from mb exit-queue prn)))))
