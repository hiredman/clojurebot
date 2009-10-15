(ns hiredman.schedule
    (:import (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)))
 
(def unit {:minutes TimeUnit/MINUTES :seconds TimeUnit/SECONDS :hours TimeUnit/HOURS})

(def tasks (ref {}))

(def #^{:doc "ScheduledThreadPoolExecutor for scheduling repeated/delayed tasks"}
     task-runner (ScheduledThreadPoolExecutor. (+ 1 (.availableProcessors (Runtime/getRuntime)))))

(defn fixedrate
  ([{:keys [name task start-delay rate unit]}]
   (fixedrate name task start-delay rate unit))
  ([name task t1 t2 tu]
   (let [ft (.scheduleAtFixedRate task-runner #^Callable task (long t1) (long t2) tu)]
     (dosync (alter tasks assoc name ft)))))

(defn cancel [name]
      (.cancel (get @tasks name) true)
      (dosync
        (alter tasks dissoc name)))

;; example usage
;; (fixedrate
;;  {:task #(dump-dict-is config) :start-delay 1 :rate 10 :unit (:minutes unit)})
