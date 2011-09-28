(ns hiredman.clojurebot.sb
  (:use (hiredman.clojurebot core)
        (hiredman sandbox)
        [clojure.tools.logging :only [info]]))

(let [cl-cache (atom {})]
  (defn cl [clojure-jar]
    (if-let [[ctime cl] (get @cl-cache clojure-jar)]
      (if (> (- (System/currentTimeMillis)
                (* 10 60 1000))
             ctime)
        (do
          (swap! cl-cache dissoc clojure-jar)
          (recur clojure-jar))
        cl)
      (doto (if clojure-jar
              (binding [*secure?* false]
                (java.security.AccessController/doPrivileged
                 (reify
                   java.security.PrivilegedAction
                   (run [_]
                     (info "new classloader")
                     (let [bootcp clojure-jar
                           cp (.split bootcp ":")
                           cp (for [c cp] (java.net.URL.
                                           (format "file://%s" c)))
                           cp (into-array java.net.URL cp)]
                       (java.net.URLClassLoader. cp nil))))))
              (.getClassLoader clojure.lang.RT))
        ;; make sure RT is loaded and inited before we try and use it
        ;; in the sandbox
        ((fn [cl]
           (java.security.AccessController/doPrivileged
               (reify
                 java.security.PrivilegedAction
                 (run [_]
                   (binding [*secure?* false]
                     (evil cl "(+ 1 2)")))))))
        ((fn [cl]
           (swap! cl-cache assoc clojure-jar
                  [(System/currentTimeMillis) cl])))))))

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
