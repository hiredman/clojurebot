(ns hiredman.clojurebot.seenx
  (:use (hiredman.clojurebot core)))

(defmethod responder ::seenx [bot msg]
  (let [nick (re-find #"[^ ]+" (.replaceFirst (extract-message bot msg) "seen " ""))]
    (sendMsg-who bot msg (if-let [where (see-nick? bot nick)] (str "I see " nick " on " where) (str "I don't see " nick " anywhere")))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"seen .*[^ ]" (:message msg)))) ::seenx)
