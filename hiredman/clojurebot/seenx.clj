(ns hiredman.clojurebot.seenx
  (:use (hiredman.clojurebot core)))

(def user-db (ref {}))

(defmethod responder ::seenx [bot msg]
  (let [nick (re-find #"[^ ]+" (.replaceFirst (extract-message bot msg) "seen " ""))]
    (if (@user-db nick)
      (sendMsg-who bot msg (str nick " was last seen saying: " (@user-db nick)))
      (sendMsg-who bot msg (str "no, I have not seen " nick)))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"seen .*[^ ]" (:message msg)))) ::seenx)

(defmethod responder ::watcher [bot msg]
  (dosync
    (alter user-db assoc (:sender msg)
           (cond
             (:join msg)
              :join
             (:part msg)
              :part
             :else
              (:message msg))))
  #(responder bot (assoc msg ::ignore true)))


(add-dispatch-hook -31 (dfn (nil? (::ignore msg))) ::watcher)
