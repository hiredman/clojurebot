; Subversion announcements

(ns hiredman.clojurebot.svn
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util])
  (:import (java.util.concurrent TimeUnit)))

(defn summary
      "takes output of clojure.xml/parse on svn's xml log, returns
      a vector of [rev-number commit-message]"
      [tag-map]
      (map (fn [x]
               [(Integer/parseInt (:revision (:attrs x)))
                (first
                  (:content
                    (first
                      (filter #(= (:tag %) :msg)
                              (:content x)))))])
           (:content tag-map)))

(def latest-revisions
     (comp summary
           clojure.xml/parse
           #(.getInputStream %)
           util/shell
           (partial str "svn -v --xml --limit 5 log ")))

(def revision
     (comp first
           summary
           clojure.xml/parse
           #(.getInputStream %)
           shell
           #(str "svn -v --xml -r " %2 " log " %)))

(def revision-cached (memoize revision))

(def latest (atom {}))

(defn start-svn-watcher [bot name url callback]
      (.scheduleAtFixedRate core/task-runner
                            (fn []
                                (try
                                (println name " checking SVN revs")
                                (when-let [revs (seq (filter #(> (first %) (get @latest name 0)) (latest-revisions url)))]
                                        (swap! latest assoc name (first (last (sort-by first revs))))
                                        (callback bot revs))
                                (catch Exception e
                                       (.printStackTrace e))))
                            (long 1)
                            (long 5)
                            TimeUnit/MINUTES))

(defn clojure-channel-helper-callback
      [bot revs]
      (doseq [r revs]
             (doseq [c (.getChannels (:this bot))]
                    (core/send-out :notice bot c (str "r" (first r) " " (second r)))))
      (core/is! "latest" (.toString (first (last (sort-by first revs))))))

(def default-repo (atom ""))

(defmethod core/responder ::svn-rev-lookup [bot pojo]
  (let [r (Integer/parseInt (re-find #"[0-9]+" (:message pojo)))]
    (core/send-out :notice bot (core/who pojo) (let [x (revision-cached @default-repo r)]
                                                 (str "r" (first x) " " (second x))))))

(core/add-dispatch-hook (core/dfn (re-find #"^r[0-9]+$" (:message msg))) ::svn-rev-lookup)
