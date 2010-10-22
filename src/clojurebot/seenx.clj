(ns clojurebot.seenx
  (:use [hiredman.clojurebot.core :only [befuddled]])
  (:import (java.util Date)))

(defonce user-db (atom {}))

(defn log-user [{:keys [message sender channel target type]}]
  (let [target (or channel sender target)]
    (swap! user-db
           update-in [target sender]
           (constantly
            [type (Date.) message]))))

(defn last-seen-str [nick channel [type time message]]
  (println "@last-seen-str")
  (println nick channel type time message)
  (let [now (java.util.Date.)
        minutes (int (/ (/ (- (.getTime now) (.getTime (or time now))) 1000) 60))]
    (condp = type
     :join
     (str nick " was last seen joining " channel ", " minutes " minutes ago" )
     :part
     (str nick " was last seen parting " channel ", " minutes " minutes ago")
     :quit
     (str nick " was last seen quiting IRC, " minutes " minutes ago")
     :message
     (str nick " was last seen in " channel ", " minutes " minutes ago saying: " message))))

(defn seen-user [{:keys [message sender channel bot]}]
  (println message sender channel bot)
  (if channel
    (let [who (.replaceAll
               (.trim (.replaceAll message "^seen " ""))
               "\\?$" "")
          last (get-in @user-db [channel who])]
      (cond
       (= who (.getNick bot))
       "of course I've seen myself"

       (seq last)
       (last-seen-str who channel last)

       :else
       (befuddled)))
    (befuddled)))

(defn seenx-query? [{:keys [message]}]
  (and message
       (re-find #"seen .*[^ ]" message)))
