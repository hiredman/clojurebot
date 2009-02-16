; Subversion announcements

(ns hiredman.clojurebot.svn
  (:use (hiredman.clojurebot core))
  (:import (java.util.concurrent TimeUnit)))

(def svn-rev-cache (ref []))

(defn send-svn-revs [bot revs]
      (doseq [i (.getChannels (:this bot))]
             (dorun
               (map #(sendMsg (:this bot) i (str "svn rev " (first %) "; " (last %)))
                    revs))))

(defn cache-svn-rev
      "puts an svn rev into the cache"
      [rev]
      (dosync (commute svn-rev-cache (comp distinct conj) rev)))

(defn svn-command [url] 
  (if url 
    (str "svn -v --xml --limit 5 log " url)
    (throw (IllegalArgumentException. "bot configuration must contain key :svn-url"))))

(defn svn-summaries
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

(defn get-last-svn-rev []
  (let [latest (@dict-is "latest")]
    (if latest
      (Integer/parseInt latest)
      0)))

(defn filter-newer-svn-revs [revs]
      (filter #(> (first %) (get-last-svn-rev))
              revs))



(defn svn-message
      "takes a seq of vectors containing [rev msg]
      sends out messages about new revs. updates \"latest\" 
      to latest rev"
      [bot summaries]
      (let [newrevs (filter-newer-svn-revs (reverse summaries))]
        (when newrevs
          (do
            (send-svn-revs bot newrevs)
            (dosync
              (commute dict-is
                       assoc
                       "latest"
                       (str (first (first summaries)))))
            ;don't want to see the whole hash in the repl
            nil))))

(defn svn-xml-stream
      "get the xml stream from svn"
      [cmd]
      (.getInputStream (.. Runtime getRuntime (exec cmd))))


(defmethod responder ::svn-rev-lookup [bot pojo]
  (let [r (Integer/parseInt (re-find #"[0-9]+" (:message pojo)))
        t (filter #(= (first %) r)  @svn-rev-cache)]
    (if (not= 0 (count t))
      (send-svn-revs bot t)
      (let [cmd (.replace (svn-command (:svn-url bot)) "--limit 5" (str "-r " r))
            b (svn-summaries (clojure.xml/parse (svn-xml-stream cmd)))]
        (do
          (send-svn-revs bot b)
          (dorun (map cache-svn-rev b)))))))

(add-dispatch-hook (dfn (re-find #"^svn rev [0-9]+$" (:message msg))) ::svn-rev-lookup)

(defn svn-notify [config]
      (println "Checking SVN revisions")
      (let [m (svn-summaries (clojure.xml/parse (svn-xml-stream (svn-command (:svn-url config)))))]
                      (svn-message config m)
                      (doall (map cache-svn-rev m))))

(defn start-svn-notifier-thread [bot]
      (.scheduleAtFixedRate task-runner2
                            #(try (svn-notify bot)
                                  (catch Exception e
                                         (println e)))
                            (long 0)
                            (long 5)
                            TimeUnit/MINUTES))
;;;;;
(defn shell [cmd]
      (.. Runtime getRuntime (exec cmd)))

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
           shell
           (partial str "svn -v --xml --limit 5 log ")))

(def revision
     (comp first
           summary
           clojure.xml/parse
           #(.getInputStream %)
           shell
           #(str "svn -v --xml -r " %2 " log " %)))

(def revision-cached (memoize revision))

(revision-cached "http://clojure.googlecode.com/svn/trunk/" 1279)
