(ns hiredman.clojurebot.sb
  (:use (hiredman.clojurebot core)
        (hiredman sandbox)))

(defn naughty-forms? [strang]
  (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot" "java.lang.Thread."}]
    (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

(defn eval-request? [{:keys [message]}]
  (and message (re-find #"^," (.trim message))))

(defn eval-message [{:keys [message sender config] :as bag}]
  (if (and (not (naughty-forms? message))
           (not= sender "itistoday")
           (not= sender "Lajla")
           (not= sender "LauJensen"))
    (let [result (eval-in-box (.replaceAll message "^," "")
                              (:sandbox-ns config 'sandbox))]
      (if (vector? result)
        result
        (.replace (str result) "(NO_SOURCE_FILE:0)" "")))
    (str sender ": " (befuddled))))
