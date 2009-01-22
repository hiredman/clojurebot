(ns hiredman.clojurebot.tinyurl
  (:use (hiredman.clojurebot core)))

;; (defn roll-die [sides]
;;       (randth (range 1 (inc sides))))
;; 
;; (defmethod responder ::roll-dice [bot pojo]
;;   (let [msg (:message pojo)
;;         dice-count (Integer/parseInt (re-find #"[0-9]+" msg))
;;         dice-type  (Integer/parseInt (.replaceAll (re-find #"d[0-9]+" msg) "[a-zA-z]" ""))
;;         modifier (try (Integer/parseInt (.replaceAll (re-find #"[+-][0-9]+" msg) "[+-]" ""))
;;                       (catch Exception e 0))]
;;     (sendMsg-who bot pojo
;;                  (+ modifier
;;                     (reduce +
;;                             (map (fn [_] (roll-die dice-type))
;;                                  (range 1 (inc dice-count))))))))
;; 
;; (add-dispatch-hook (dfn (re-find #"^[0-9]+d[0-9]+" (:message msg))) ::roll-dice)

(def url-reg #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

(defn getTinyUrl [url]
      (with-open [pt (.getContent (java.net.URL. (java.net.URLEncode/encode url)))
                  dis (java.io.DataInputStream. pt)]
                 (.readLine dis)))

(defmethod responder ::tiny-url [bot pojo]
  (println (getTinyUrl (re-find url-reg (:message pojo)))))


(add-dispatch-hook 20 (dfn (re-find url-reg (:message msg))) ::tiny-url)


