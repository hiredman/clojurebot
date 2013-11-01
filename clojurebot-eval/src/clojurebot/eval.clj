(ns clojurebot.eval
  (:require [clojurebot.sandbox :as sb]
            [clojure.tools.logging :as log]
            [ring.middleware.params :refer [wrap-params]]
            [clojure.stacktrace :as s]))

(defn handler* [{{:strs [expression befuddled]} :params :as m}]
  (if-not (or (and (.contains expression "shadow")
                   (.contains expression "worship"))
              (and (.contains expression "Shadow")
                   (.contains expression "Divine")))
    (try
      (let [r (sb/eval-message expression (read-string befuddled))
            [stdout stderr result]  (if (vector? r)
                                      r
                                      [nil r nil])]
        {:status 200
         :body (pr-str {:stdout stdout
                        :stderr stderr
                        :result result})})
      (catch Throwable t
        (log/error t "error evaluating" expression)
        (log/error (pr-str m))
        {:status 200
         :body (pr-str {:stdout ""
                        :stderr (print-str
                                 (try
                                   (s/root-cause t)
                                   (catch Throwable _
                                     t)))
                        :result ""})}))
    {:status 500
     :body ""}))

(def handler (-> #'handler*
                 wrap-params))

(defn init []
  (when (empty? (System/getProperty "java.security.policy"))
    (System/setProperty
     "java.security.policy"
     (str (.getResource (class #'init) "/example.policy")))))
