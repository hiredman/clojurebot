(ns hiredman.clojurebot.sb
  (:use (hiredman.clojurebot core)
        (hiredman sandbox)))

(defn naughty-forms? [strang]
      (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot" "java.lang.Thread."}]
        (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

(defmethod responder ::code-sandbox [bot pojo]
  (log (select-keys pojo [:sender :message]))
  (if (and (not (naughty-forms? (:message pojo)))
           (not= (:sender pojo) "itistoday")
           (not= (:sender pojo) "Lajla"))
    (let [result (try (eval-in-box (.replaceAll (:message pojo) "^," "")
                              (:sandbox-ns bot 'sandbox))
                      (catch Exception e
                        (str "Eval-in-box threw an exception:"
                             (.getMessage e))))]
      (if (vector? result)
        (doseq [i (reverse result)]
          (new-send-out bot :msg pojo (str i)))
        (new-send-out bot :msg pojo (.replace (str result) "(NO_SOURCE_FILE:0)" ""))))
  (new-send-out bot :msg pojo (str (:sender pojo) ": " (befuddled)))))


(add-dispatch-hook  (dfn (re-find #"^," (:message msg))) ::code-sandbox)
