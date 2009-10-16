(ns hiredman.clojurebot.seenx
  (:use (hiredman.clojurebot core))
  (:require [hiredman.schedule :as sched]))

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

(defresponder ::seenx 0
  (dfn (and (:addressed? (meta msg))
            (re-find #"seen .*[^ ]" (:message msg))))
  (let [nick (.replaceAll
               (.replaceAll (d?op (extract-message bot msg)) "(?:.*) seen ([^ ])" "$1")
               "^seen " "")]
    (if (.equals (:nick bot) nick)
      (sendMsg-who bot msg "Of course I have seen myself.")
      (if (@user-db nick)
        (sendMsg-who bot msg (last-seen-str nick (@user-db nick)))
        (sendMsg-who bot msg (str "no, I have not seen " nick))))))

;(remove-dispatch-hook ::seenx)

(defn user-watch [bot]
      (let [cur (count (.getUsers (:this bot) "#clojure"))
            pre (Integer/parseInt (:object (first (what-is "max people" bot))))]
        (when (> cur pre)
          (store bot "max people" (str cur)))))

(defresponder ::watcher -31
  (dfn (nil? (::watcher (meta msg))))
  (when (:join msg)
    (user-watch bot))
  (dosync (commute user-db assoc (:sender msg)
                   (vector
                     (cond
                       (:join msg)
                        :join
                       (:part msg)
                        :part
                       (:quit msg)
                        :quit
                       :else
                        (:message msg))
                     (:channel msg)
                     (:time (bean (java.util.Date.))))))
  (when (and (not (:part msg)) (:message msg)) (swap! activity inc))
  #(responder bot msg))
                     
;(remove-dispatch-hook ::watcher)

(defn lower-activity []
      (try (swap! activity (fn [x] (if (> x 0) (dec x) 0)))
                            (catch Exception e
                                   (.printStackTrace e))))

(sched/fixedrate {:name ::activity :task lower-activity :start-delay 60 :rate 30 :unit (:seconds sched/unit)})
