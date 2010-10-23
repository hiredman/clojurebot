(ns clojurebot.dice)

(defn roll-die [sides]
  (first (shuffle (range 1 (inc sides)))))

(defn dice-roll? [{:keys [message]}]
  (and message
       (re-find #"^[0-9]+d[0-9]+" message)))

(defn roll-some-dice [{:keys [message]}]
  (let [dice-count (Integer/parseInt (re-find #"[0-9]+" message))
        dice-type  (Integer/parseInt (.replaceAll (re-find #"d[0-9]+" message) "[a-zA-z]" ""))
        modifier (try (Integer/parseInt (.replaceAll (re-find #"[+-][0-9]+" message) "[+-]" ""))
                      (catch Exception e 0))]
    (str
     (+ modifier
        (reduce +
                (map (fn [_] (roll-die dice-type))
                     (range 1 (inc dice-count))))))))
