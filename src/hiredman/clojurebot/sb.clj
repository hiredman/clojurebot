(ns hiredman.clojurebot.sb
  (:use (hiredman.clojurebot core)
        (hiredman sandbox)))

(defonce cl
  (memoize
   (fn [clojure-jar]
     (doto (if clojure-jar
             (let [bootcp clojure-jar
                   cp (.split bootcp ":")
                   cp (for [c cp] (java.net.URL. (format "file://%s" c)))
                   cp (into-array java.net.URL cp)]
               (java.net.URLClassLoader. cp nil))
             (.getClassLoader clojure.lang.RT))
       ;; make sure RT is loaded and inited before we try and use it
       ;; in the sandbox
       (evil "(+ 1 2)")))))

(defn naughty-forms? [strang]
  (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot"
             "java.lang.Thread."}]
    (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

(defn eval-request? [{:keys [message]}]
  (and message (re-find #"^," (.trim message))))

(defn eval-message [{:keys [message sender config] :as bag}]
  (if (and (not (naughty-forms? message))
           (not= sender "itistoday")
           (not= sender "Lajla")
           (not= sender "LauJensen"))
    (let [result (eval-in-box (.replaceAll message "^," "")
                              (:sandbox-ns config 'sandbox)
                              (cl (config :clojure-jar)))]
      (if (vector? result)
        result
        (.replace (str result) "(NO_SOURCE_FILE:0)" "")))
    (str sender ": " (befuddled))))
