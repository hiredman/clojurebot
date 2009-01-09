(ns hiredman.clojurebot-dice
  (:use (hiredman clojurebot-core)))


(defn roll-die [sides]
      (randth (range 1 (inc sides))))

;; (let [a (java.util.LinkedList. (range 1 7))] (java.util.Collections/shuffle a) (first a))


(defn roll-die [sides]
      (let [a (java.util.LinkedList. (range 1 (inc sides)))
            _ (java.util.Collections/shuffle a)]
        (first a)))


(defmethod responder ::roll-dice [bot pojo]
  (let [msg (:message pojo)
        dice-count (Integer/parseInt (re-find #"[0-9]+" msg))
        dice-type  (Integer/parseInt (.replaceAll (re-find #"d[0-9]+" msg) "[a-zA-z]" ""))
        modifier (try (Integer/parseInt (.replaceAll (re-find #"[+-][0-9]+" msg) "[+-]" ""))
                      (catch Exception e 0))]
    (sendMsg-who bot pojo
                 (+ modifier
                    (reduce +
                            (map (fn [_] (roll-die dice-type))
                                 (range 1 (inc dice-count))))))))

(add-dispatch-hook (dfn (re-find #"^[0-9]+d[0-9]+" (:message msg))) ::roll-dice)
