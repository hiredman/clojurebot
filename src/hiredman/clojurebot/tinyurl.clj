(ns hiredman.clojurebot.tinyurl
  (:use (hiredman.clojurebot core)))

(def url-reg #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

(defn get-tiny-url [url]
      (with-open [pt (.getContent (java.net.URL. (str "http://tinyurl.com/api-create.php?url=" (java.net.URLEncoder/encode url))))
                  dis (java.io.DataInputStream. pt)]
                 (.readLine dis)))

(def get-tiny-url-cached (memoize get-tiny-url))

(defmethod responder ::tiny-url [bot pojo]
  (let [url (re-find url-reg (:message pojo))]
    (when (> (count url) 60)
      (try (.sendNotice (:this bot) (who pojo) (get-tiny-url-cached url)) (catch Exception e (println e))))))


(add-dispatch-hook 20 (dfn (re-find url-reg (:message msg))) ::tiny-url)
