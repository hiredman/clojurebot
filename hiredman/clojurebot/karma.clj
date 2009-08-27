(ns hiredman.clojurebot.karma
  (:use (hiredman.clojurebot core)))

(defn increment-karma [name bot]
  (send (:store bot)
        update-in [:karma name] inc))

(defn decrement-karma [name bot]
  (send (:store bot)
        update-in [:karma name] dec))

(defn get-karma [name bot]
  (-> bot :store deref :karma (get name)))

(defn setup-karma [bot]
  (send (:store bot) assoc :karma {}))
