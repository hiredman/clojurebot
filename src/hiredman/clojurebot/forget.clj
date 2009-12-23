(ns hiredman.clojurebot.forget
    (:use (hiredman.clojurebot core)))

(defmethod responder ::forget [bot junks]
  (dosync
    (alter dict-is dissoc (.trim (.replaceAll (extract-message bot junks) "^forget (.*)" "$1"))))
  (sendMsg-who bot junks (str "I forgot " (.trim (.replaceAll (extract-message bot junks) "^forget (.*)" "$1")))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^forget " (extract-message bot msg)))) ::forget)
