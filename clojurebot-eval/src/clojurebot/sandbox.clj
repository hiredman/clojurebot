(ns clojurebot.sandbox
  (:use [clojure.tools.logging :only [info]])
  (:require [carica.core :refer [config]])
  (:import (java.util.concurrent FutureTask TimeUnit TimeoutException)
           (java.io File FileWriter PushbackReader StringReader)))

(def *bad-forms*
  #{'alter-var-root
    'alterRoot
    'intern
    'eval
    'def
    'catch
    'load-string
    'load-reader
    'clojure.core/addMethod
    'hiredman.clojurebot/bot})

(def default-timeout 10) ; in seconds

(definterface Door
  (lock [pw])
  (unlock [pw])
  (isLocked []))

(defmacro defering-security-manager [sm door-key & [m]]
  (let [sm-name (gensym 'sm)
        methods (for [[method-name methods]
                      (group-by #(.getName %)
                                (.getDeclaredMethods SecurityManager))
                      :when (.startsWith method-name "check")
                      :let [methods (filter
                                     #(java.lang.reflect.Modifier/isPublic
                                       (.getModifiers %))
                                     methods)]]
                  (if (contains? m method-name)
                    (get m method-name)
                    `(~(symbol method-name)
                      ~@(for [[argc [method]] (group-by
                                               #(count (.getParameterTypes %))
                                               methods)
                              :let [args (vec (map-indexed
                                               (comp symbol str)
                                               (take argc (repeat "arg"))))]]
                          `(~args
                            (when (.isLocked ~'this)
                              (println ~method-name ~@args)
                              (throw (java.lang.SecurityException.
                                      "denied"))))))))]
    `(let [~sm-name ~sm
           ~'b (proxy [java.lang.InheritableThreadLocal] []
                 (initialValue []
                   false))]
       (proxy [SecurityManager Door] []
         ~@methods
         (isLocked []
           (.get ~'b))
         (lock [pw#]
           (when (= pw# ~door-key)
             (.set ~'b true)))
         (unlock [pw#]
           (when (= pw# ~door-key)
             (.set ~'b false)))))))

(defn enable-security-manager [k]
  (info "enable-security-manager")
  (System/setSecurityManager
   (let [sm (SecurityManager.)
         m (defering-security-manager sm k
             {"checkPackageAccess"
              (checkPackageAccess [package])
              "checkMemberAccess"
              (checkMemberAccess
               [class member]
               (when (.isLocked ^Door this)
                 (when (= class Runtime)
                   (throw (java.lang.SecurityException.
                           "Reference To Runtime is not allowed")))
                 (when (= class java.security.PrivilegedAction)
                   (throw (java.lang.SecurityException.
                           "Reference to PrivilegedActions is not allowed")))))
              "checkCreateClassLoader"
              (checkCreateClassLoader [])
              "checkPermission"
              (checkPermission
               [p]
               (when (.isLocked ^Door this)
                 (if (instance? java.lang.reflect.ReflectPermission p)
                   nil
                   (proxy-super checkPermission p))))
              "checkAccess"
              (checkAccess
               [t-or-tg]
               (when (.isLocked ^Door this)
                 (throw (java.lang.SecurityException. "no threads please"))))})]
     m)))



;;;;;;;; Chousuke
(defn thunk-timeout [thunk seconds]
  (let [task (FutureTask. thunk)
        thr (Thread. task)]
    (try
      (.start thr)
      (.get task seconds TimeUnit/SECONDS)
      (catch TimeoutException e
        (.cancel task true)
        (.stop thr (Exception. "Thread stopped!"))
        (pr-str "Execution Timed Out")))))

(defn wrap-exceptions
  ([f]
     (wrap-exceptions f #(do
                           (.printStackTrace %)
                           (.getMessage %))))
  ([f exception-handler]
     (try (f) (catch Exception e (exception-handler e)))))
;;;;;;;;;;;

(defn empty-perms-list []
  (java.security.Permissions.))

(defn domain [perms]
  (java.security.ProtectionDomain.
   (java.security.CodeSource.
    nil
    (cast java.security.cert.Certificate nil))
   perms))

(defn context [dom]
  (java.security.AccessControlContext.
   (into-array [dom])))

(defn priv-action [thunk]
  (reify java.security.PrivilegedAction
    (run [_] (thunk))))

(defn sandbox [thunk context]
  (java.security.AccessController/doPrivileged
   (priv-action thunk)
   context))

(defn write-test []
  (doto (-> "foo.txt" File. FileWriter.) (.write "foo") .close))

(defn de-fang
  "looks through the macroexpand of a form for things I don't allow"
  [form notallowed]
  (if (coll? form)
    (when (not
           (some notallowed
                 (tree-seq coll?
                           #(let [a (macroexpand %)]
                              (if (coll? a)
                                (seq a)
                                (list a)))
                           form)))
      form)
    form))

(defn cond-eval [pred form]
  (if (pred form)
    (eval form)
    (throw (java.lang.Exception. "DENIED"))))

(defn killall-thrdgrp [thg]
  (let [a (make-array Thread (.activeCount thg))
        _ (.enumerate thg a true)]
    (map #(.stop % (Exception. "KILLED")))
    (.destroy thg)))

(defn save-to-gensym [x]
  (let [a (gensym)
        b (list 'def a x)]
    (eval b)
    (str a)))

(defn my-doc [befuddled]
  (let [arg-name (gensym)]
    (list 'defmacro 'my-doc [arg-name]
          `(let [m# (meta (resolve ~arg-name))
                 al# (:arglists m#)
                 docstring# (:doc m#)]
             (if m#
               (.replaceAll (str al# "; " docstring# ) "\\s+" " ")
               ''~befuddled)))))

(defn force-lazy-seq
  "if passed a lazy seq, forces seq with doall, if not return what is passed"
  [s]
  (or (and (instance? clojure.lang.LazySeq s) (doall s)) s))

(defn eval-in-box-helper [form {:keys [print-length print-level]}]
  (let [result (cond-eval #(de-fang % *bad-forms*) form)]
    (.close *out*)
    (.close *err*)
    (binding [*print-length* print-length
              *print-level* print-level]
      (let [r (prn-str (force-lazy-seq result))
            o (print-str (.toString *out*))
            e (print-str (.toString *err*))]
        [o e (when (or result (.equals o "")) r)]))))

(defn call-method
  "Calls a private or protected method.

   params is a vector of classes which correspond to the arguments to
   the method e

   obj is nil for static methods, the instance object otherwise.

   The method-name is given a symbol or a keyword (something Named)."
  [klass method-name params obj & args]
  (-> klass (.getDeclaredMethod (name method-name)
                                (into-array Class params))
      (.invoke obj (into-array Object args))))


(defprotocol Evaluator
  (evil [evaluator form]))

(extend-type ClassLoader
  Evaluator
  (evil [cl form-str]
    (read-string
     (let [old-cl (.getContextClassLoader (Thread/currentThread))]
       (try
         (.setContextClassLoader (Thread/currentThread) cl)
         (let [rt (.loadClass cl "clojure.lang.RT")
               compiler (.loadClass cl "clojure.lang.Compiler")
               var- (fn [s]
                      (call-method
                       rt :var [String String] nil (namespace s) (name s)))
               class (fn [x] (.loadClass cl (name x)))
               deref (fn [x] (call-method (.getClass x) :deref [] x))
               invoke (fn [x &  args] (call-method (.getClass x) :invoke []))
               read-string (fn [s]
                             (call-method rt :readString [String] nil s))
               eval (fn [f]
                      (call-method compiler :eval [Object] nil f))]
           (thunk-timeout
            (fn []
              (sandbox #(eval (read-string (format "(pr-str %s)" form-str)))
                       (context (domain (empty-perms-list)))))
            default-timeout))
         (finally
          (.setContextClassLoader (Thread/currentThread) old-cl)))))))

(defn eval-in-box [_string sb-ns class-loader n befuddled]
  (enable-security-manager n)
  (let [f `(do
             (with-open [o# (java.io.StringWriter.)
                         e# (java.io.StringWriter.)]
               (binding [*out* o#
                         *err* e#
                         *read-eval* false
                         *print-level* 10
                         *print-length* 5
                         *ns* (find-ns 'clojure.core)]
                 ~(my-doc befuddled)
                 (ns ~sb-ns
                   (:use [clojure.repl]))
                 (alter-var-root (resolve '~'doc)
                                 (constantly (resolve '~'my-doc)))
                 (.lock (System/getSecurityManager) ~n)
                 (try
                   (let [f# (read-string ~_string)
                         good?# (if (and (coll? f#)
                                         (not (empty? f#)))
                                  (when (not
                                         (some '~*bad-forms*
                                               (tree-seq coll?
                                                         (fn [i#]
                                                           (let [a# (macroexpand
                                                                     i#)]
                                                             (if (coll? a#)
                                                               (seq a#)
                                                               (list a#))))
                                                         f#)))
                                    f#)
                                  true)
                         r# (pr-str (try
                                      (when-not good?#
                                        (throw (Exception. "SANBOX DENIED")))
                                      (eval f#)
                                      (catch Throwable t#
                                        t#)))]
                     [(.toString (doto o# .close))
                      (.toString (doto e# .close))
                      r#])
                   (finally
                     (.unlock (System/getSecurityManager) ~n))))))
        thunk (fn [] (evil class-loader f))]
    (thunk)))


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
                     (java.net.URLClassLoader. cp nil)))))
              (.getClassLoader clojure.lang.RT))
        ;; make sure RT is loaded and inited before we try and use it
        ;; in the sandbox
        ((fn [cl]
           (java.security.AccessController/doPrivileged
            (reify
              java.security.PrivilegedAction
              (run [_]
                (try
                  (evil cl "(+ 1 2)")
                  (catch Exception e
                    (swap! cl-cache dissoc clojure-jar)
                    (throw e))))))))
        ((fn [cl]
           (swap! cl-cache assoc clojure-jar
                  [(System/currentTimeMillis) cl])))))))

(defn naughty-forms? [strang]
  (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot"
             "java.lang.Thread."}]
    (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

(defn eval-request? [{:keys [message]}]
  (and message (re-find #"^," (.trim message))))

(let [n (str (java.util.UUID/randomUUID))]
  (defn eval-message [expression befuddled]
    (let [result (eval-in-box expression
                              (or (config :sandbox-ns) 'sandbox)
                              (cl (config :clojure-jar))
                              n
                              befuddled)]
      (if (vector? result)
        result
        (.replace (str result) "(NO_SOURCE_FILE:0)" "")))))
