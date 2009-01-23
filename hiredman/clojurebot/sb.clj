(ns hiredman.clojurebot.sb
  (:use (hiredman.clojurebot core)
        (hiredman sandbox)))

(defn naughty-forms? [strang]
      (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot"}]
        (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

(defmethod responder ::code-sandbox [bot pojo]
  (println (str (:sender pojo) " " (:message pojo)))
  (if (not (naughty-forms? (:message pojo)))
    (let [result (try (eval-in-box (.replaceAll (:message pojo) "^," "")
                              (:sandbox-ns bot))
                      (catch Exception e
                             (str "Eval-in-box threw an exception:" (.getMessage e))))
          _ (println "Result:" result)]
      (if (vector? result)
        (doseq [i (reverse result)]
           (sendMsg-who bot pojo i))
        (sendMsg-who bot pojo (.replace result "(NO_SOURCE_FILE:0)" ""))))
  (sendMsg-who bot pojo (befuddled))))



(add-dispatch-hook  (dfn (re-find #"^,\(" (:message msg))) ::code-sandbox)
