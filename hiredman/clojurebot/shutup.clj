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
        (alter mute assoc channel true))
      (println "Mute in " channel))

(defn unmute-in [channel]
      (dosync
        (alter mute assoc channel nil))
      (println "Unmute in " channel))

(defmethod responder ::shutup [bot msg]
  (mute-in (:channel msg))
  (.schedule task-runner
             (make-timer-task
               #(unmute-in (:channel msg)))
             (long (* 60 1000))))

;;   (send-off (agent nil)
;;             (fn [& _]
;;                 (Thread/sleep (* 60 1000))
;;                 (unmute-in (:channel msg)))))
 
(add-dispatch-hook (dfn (and (addressed? bot msg) (re-find #"shut up$" (:message msg)))) ::shutup)
