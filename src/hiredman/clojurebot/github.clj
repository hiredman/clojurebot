(ns hiredman.clojurebot.github
 (:import (java.text SimpleDateFormat ParsePosition FieldPosition))
 (:require [hiredman.clojurebot.core :as core]
           [hiredman.schedule :as sched]
           [clojurebot.json :as json]
           [hiredman.utilities :as util]))

(def api-url "http://github.com/api/v2/json/commits/list/richhickey/clojure/master")

(def commit-url "http://github.com/api/v2/json/commits/show/richhickey/clojure")

(defn parse-date
  "Git Hub Date String -> Java Date Object"
  [string]
  (.parse (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssZ")
          (.replaceAll string "(\\d\\d):(\\d\\d)$" "$1$2")
          (ParsePosition. 0)))

(defn deparse-date [date]
  (.toString
    (.format (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssZ")
             date
             (StringBuffer.)
             (FieldPosition. 0))))

(defn get-commit
  "get the commit message for commit n"
  [n]
  (-> commit-url (str "/" n) util/get-url json/decode-from-str :commit :message))

(core/defresponder ::git-commit 0
  (core/dfn (and (re-find #"^git ([^ ])" (core/extract-message bot msg))
                 (:addressed? (meta msg)))) ;;
  (let [m (.replaceAll (core/extract-message bot msg) (.toString #"^git ([^ ])") "$1")]
    (core/new-send-out bot :msg msg (get-commit m))))

(defn get-commits
  "get list of commits from github"
  []
  (->  api-url util/get-url json/decode-from-str :commits
       ((partial map #(update-in % [:committed_date] parse-date)))
       ((partial map #(update-in % [:authored_date] parse-date)))
       ((partial sort-by :commited_date))
       reverse))

(defn new-commit?
  "is this date after the date of the last commit this bot has a record for?"
  [date bot]
  (if-let [d (get (deref (:store bot)) "last commit")]
    (.before (parse-date d) date)
    true))

(defn set-commit!
  "record this date in this bot's store so we know that commits after this date are new"
  [date bot]
  (send-off (:store bot) assoc "last commit" (deparse-date date)))

(defn format-commit
  "format commit for sending"
  [commit]
  (str (:name (:author commit)) ": " (:message commit)
       "; " (util/tinyurl (:url commit))))

(defn format-and-store
  "store date from this commit and then return the commit formated for sending out"
  [commit bot]
  (set-commit! (:committed_date commit) bot)
  (format-commit commit))

(defn process-commits
  "process commits for bot and send results to channel"
  [bot channel]
  (println "processing commits")
  (doseq [c (filter #(-> % :committed_date (new-commit? bot)) (get-commits))]
    (core/new-send-out bot :notice channel (format-and-store c bot))))

(defn start-github-watch [bot channel]
  (sched/fixedrate {:task #(process-commits bot channel) :start-delay 1 :rate 10 :unit (:minutes sched/unit)}))
