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

(defn evil [cl form-str]
  (read-string
   (let [old-cl (.getContextClassLoader (Thread/currentThread))]
     (try
       (.setContextClassLoader (Thread/currentThread) cl)
       (let [rt (.loadClass cl "clojure.lang.RT")
             compiler (.loadClass cl "clojure.lang.Compiler")
             var- (fn [s]
                    (wh/method
                     rt :var [String String] nil (namespace s) (name s)))
             class (fn [x] (.loadClass cl (name x)))
             deref (fn [x] (wh/method (.getClass x) :deref [] x))
             invoke (fn [x &  args] (wh/method (.getClass x) :invoke []))
             read-string (fn [s]
                           (wh/method rt :readString [String] nil s))
             eval (fn [f]
                    (wh/method compiler :eval [Object] nil f))]
         (eval (read-string (format "(pr-str %s)" form-str))))
         (finally
           (.setContextClassLoader (Thread/currentThread) old-cl))))))

(defonce server (vc/create-server))

(defn f []
  (let [l (loader)
        m (.loadModule l (ModuleIdentifier/create "clojurebot.legacy" "main"))]
    (evil (.getClassLoader m)
          (pr-str
           `(do
              (require 'clojurebot.legacy)
              (future ((resolve 'clojurebot.legacy/-main)))
              nil)))))

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
