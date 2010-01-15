(ns hiredman.clojurebot.karma
  (:use (hiredman.clojurebot core)))

(defn increment-karma [name bot]
  (send (:store bot)
        update-in [:karma name] inc))

(defn decrement-karma [name bot]
  (send (:store bot)
        update-in [:karma name] dec))

(defn get-karma [name bot]
  (let [k (-> bot :store deref :karma (get name))]
    (if k k 0)))

(defn setup-karma [bot]
  (send (:store bot) assoc :karma {}))
