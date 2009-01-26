(ns hiredman.clojurebot.shutup
  (:use (hiredman.clojurebot core)))

(def mute (ref {}))

(defn muted? [channel]
      (@mute channel))

(defmethod responder ::mute [bot msg]
  (when (not (muted? (:channel msg)))
    #(responder bot (assoc msg ::ignore true))))

(add-dispatch-hook (- (first (first @*dispatchers*)) 10)
                   (dfn (nil? (::ignore msg))) ::mute)

(defn mute-in [channel]
      (dosync
        (alter mute assoc channel true)))

(defn unmute-in [channel]
      (dosync
        (alter mute assoc channel nil)))

(defmethod responder ::shutup [bot msg]
  (mute-in (:channel msg))
  (send-off (agent nil)
            (fn [& _]
                (Thread/sleep (* 60 1000))
                (unmute-in (:channel msg)))))

(add-dispatch-hook (dfn (and (addressed? bot msg) (re-find #"shut up$" (:message msg)))) ::shutup)
