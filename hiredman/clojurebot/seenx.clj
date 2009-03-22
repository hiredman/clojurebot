(ns hiredman.clojurebot.seenx
  (:use (hiredman.clojurebot core))
  (:import (java.util.concurrent TimeUnit)))

(def user-db (ref {}))

(def activity (atom 0))

(defn last-seen-str [nick [m channel ms]]
      (let [minutes (int (/ (/ (- (:time (bean (java.util.Date.))) ms) 1000) 60))]
      (cond
        (= m :join)
          (str nick " was last seen joining " channel ", " minutes " minutes ago" )
        (= m :part)
          (str nick " was last seen parting " channel ", " minutes " minutes ago")
        (= m :quit)
          (str nick " was last seen quiting IRC, " minutes " minutes ago")
         :else
          (str nick " was last seen in " channel ", " minutes " minutes ago saying: " m))))

(defmethod responder ::seenx [bot msg]
  (let [nick (.replaceAll
               (.replaceAll (d?op (extract-message bot msg)) "(?:.*) seen ([^ ])" "$1")
               "^seen " "")]
    (if (.equals (:nick bot) nick)
      (sendMsg-who bot msg "Of course I have seen myself.")
      (if (@user-db nick)
        (sendMsg-who bot msg (last-seen-str nick (@user-db nick)))
        (sendMsg-who bot msg (str "no, I have not seen " nick))))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"seen .*[^ ]" (:message msg)))) ::seenx)

(defmethod responder ::watcher [bot msg]
  (dosync
    (commute user-db assoc (:sender msg)
           (vector
             (cond
               (:join msg)
                (do (user-watch (:this bot)) :join)
               (:part msg)
                :part
               (:quit msg)
                :quit
               :else
                (:message msg))
             (:channel msg)
             (:time (bean (java.util.Date.))))))
    (when (and (not (:part msg)) (:message msg)) (swap! activity inc))
  #(responder bot (vary-meta msg assoc ::ignore true)))

(add-dispatch-hook -31 (dfn (nil? (::ignore (meta msg)))) ::watcher)

(defn lower-activity []
      (try (swap! activity (fn [x] (if (> x 0) (dec x) 0)))
                            (catch Exception e
                                   (.printStackTrace e))))

(.scheduleAtFixedRate task-runner
                      lower-activity
                      (long 60)
                      (long 30)
                      TimeUnit/SECONDS)
