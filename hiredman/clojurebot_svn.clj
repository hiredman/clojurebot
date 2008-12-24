; Subversion announcements

(ns hiredman.clojurebot-svn
  (:use (hiredman clojurebot-core)))

(declare *svn-url*)

(def svn-rev-cache (ref []))


(defn send-svn-revs [revs]
      (dorun
        (map #(sendMsg *bot*
                       *channel*
                        (str "svn rev " (first %) "; " (last %)))
             revs)))

(defn cache-svn-rev
      "puts an svn rev into the cache"
      [rev]
      (dosync (commute svn-rev-cache conj rev)))

(defn get-svn-command [] (str "svn -v --xml --limit 5 log " *svn-url*))

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
      (Integer/parseInt (@dict-is "latest")))

(defn filter-newer-svn-revs [revs]
      (filter #(> (first %) (get-last-svn-rev))
              revs))



(defn svn-message
      "takes a seq of vectors containing [rev msg]
      sends out messages about new revs. updates \"latest\" 
      to latest rev"
      [summaries]
      (let [newrevs (filter-newer-svn-revs (reverse summaries))]
        (when newrevs
          (do
            (send-svn-revs newrevs)
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


(defmethod responder ::svn-rev-lookup [pojo]
  (let [r (Integer/parseInt (re-find #"[0-9]+" (:message pojo)))
        t (filter #(= (first %) r)  @svn-rev-cache)]
    (if (not= 0 (count t))
      (send-svn-revs t)
      (let [cmd (.replace (get-svn-command) "--limit 5" (str "-r " r))
            b (svn-summaries (clojure.xml/parse (svn-xml-stream cmd)))]
        (do
          (send-svn-revs b)
          (dorun (map cache-svn-rev b)))))))

(add-dispatch-hook #(re-find #"^svn rev [0-9]+$" (:message %)) ::svn-rev-lookup)


(defn svn-notifier-thread []
      (send-off (agent nil)
                (fn this [& _]
                    (let [m (svn-summaries (clojure.xml/parse (svn-xml-stream (get-svn-command))))]
                      (svn-message m)
                      (map cache-svn-rev m))
                    (Thread/sleep (* 5 60000))
                    (send-off *agent* this))))

(defn start-svn-notifier [url]
  (binding [*svn-url* url]
    (svn-notifier-thread)))
