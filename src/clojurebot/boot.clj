(ns clojurebot.boot
  (:require [wall.hack :as wh]
            [clojure.java.io :as io]
            [vespa.crabro :as vc]
            [vespa.protocols :as vp])
  (:import (org.jboss.modules LocalModuleLoader
                              ModuleIdentifier)
           (java.io File)))

(defn loader []
  (LocalModuleLoader.
   (into-array File
               [(io/file (System/getProperty "user.dir")
                         "modules")])))

(defonce server (vc/create-server))

(defn f []
  (future
    (let [l (loader)
          m (.loadModule l (ModuleIdentifier/create "clojurebot.legacy" "main"))
          cl (.getClassLoader m)]
      (let [old-cl (.getContextClassLoader (Thread/currentThread))]
        (try
          (.setContextClassLoader (Thread/currentThread) cl)
          (.run m nil)
          (finally
            (.setContextClassLoader (Thread/currentThread) old-cl)))))))

(defmacro remote-do [& forms]
  (let [reply-queue (name (gensym 'reply))]
    `(future
       (let [result# (promise)]
         (with-open [mb# (vc/message-bus)]
           (future
             (try
               (with-open [mb# (vc/message-bus)]
                 (vp/receive-from mb# ~reply-queue result#))
               (catch Throwable t#
                 (result# {:bad (pr-str t#)}))))
           (vc/send-to mb# "fnparse"
                       ~(pr-str
                         {:reply-to reply-queue
                          :payload (cons 'do forms)})))
         (let [{good# :good
                bad# :bad} (read-string @result#)]
           (if bad#
             (throw (Exception. bad#))
             good#))))))
