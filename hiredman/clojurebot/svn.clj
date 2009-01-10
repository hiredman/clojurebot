; Subversion announcements

(ns hiredman.clojurebot.svn
  (:use (hiredman.clojurebot core)))

(def svn-rev-cache (ref []))

(defn send-svn-revs [bot revs]
      (dorun
        (map #(sendMsg (:this bot)
                       (:channel bot)
                        (str "svn rev " (first %) "; " (last %)))
             revs)))

(defn cache-svn-rev
      "puts an svn rev into the cache"
      [rev]
      (dosync (commute svn-rev-cache conj rev)))

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
  (println :svn-responder)
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

(defn start-svn-notifier-thread [bot]
      (send-off (agent nil)
                (fn this [& _]
                  (println "Checking SVN revisions")
                    (let [m (svn-summaries (clojure.xml/parse (svn-xml-stream (svn-command (:svn-url bot)))))]
                      (svn-message bot m)
                      (map cache-svn-rev m))
                    (Thread/sleep (* 5 60000))
                    (send-off *agent* this))))
