(ns hiredman.http-server
  (:import (java.io InputStreamReader PrintWriter BufferedReader)
           (java.net ServerSocket URL)))

(def running true)

(defn- client-action [fn reader writer]
  (let [result (fn reader)]
    (binding [*out* writer]
      (println "HTTP/1.0 " (:status result))
      (println "Server: a Simple Server")
      (println "Content-type: " (:content-type result))
      (println "Content-length: " (-> result :body .getBytes count))
      (println "")
      (println (:body result))))
  (doseq [x [reader writer]] (.close x)))

(defn- handle-client [fn client-socket]
  (future
    (client-action fn
                   (-> client-socket .getInputStream)
                   (-> client-socket .getOutputStream PrintWriter.))))

(defn- server-action [fn server-socket]
  (when running (send-off *agent* server-action server-socket))
  (try
    (handle-client fn (.accept server-socket))
    (catch Exception e
      (prn e)))
  fn)

(defn http-server [fn portn]
  (send-off (agent fn)
            server-action
            (ServerSocket. portn)))

(defn put [stuff url]
  (let [con (doto (.openConnection (URL. url))
              (.setDoInput true)
              (.setDoOutput true))]
    (with-open [wtr (-> con .getOutputStream PrintWriter.)]
      (binding [*out* wtr] (print stuff) (flush)))
    (with-open [rdr (-> con .getInputStream InputStreamReader. BufferedReader.)]
      (apply str (line-seq rdr)))))
