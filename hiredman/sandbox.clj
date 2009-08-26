(ns hiredman.sandbox
    (:require [hiredman.clojurebot.core :as core])
    (:import (java.util.concurrent FutureTask TimeUnit TimeoutException)
             (java.io File FileWriter PushbackReader StringReader)))

;(def *bad-forms* #{'eval 'catch 'try 'def 'defn 'defmacro 'read 'Thread. 'send 'send-off 'clojure.asm.ClassWriter.})

(def *bad-forms* #{'intern 'eval 'def 'catch 'load-string 'load-reader 'clojure.core/addMethod})

(def *default-timeout* 10) ; in seconds

(defn enable-security-manager []
      (System/setSecurityManager (SecurityManager.)))

;;;;;;;; Chousuke
(defn thunk-timeout [thunk seconds]
      (let [task (FutureTask. thunk)
            thr (Thread. task)]
        (try
          (.start thr)
          (.get task seconds TimeUnit/SECONDS)
          (catch TimeoutException e
                 (.cancel task true)
                 (.stop thr (Exception. "Thread stopped!")) "Execution Timed Out"))))
 
(defn wrap-exceptions 
  ([f]
     (wrap-exceptions f #(do 
                           (println (.printStackTrace %)) 
                           (.getMessage %))))
  ([f exception-handler]
     (try (f) (catch Exception e (exception-handler e)))))
;;;;;;;;;;;

(defn empty-perms-list []
      (doto (java.security.Permissions.)
        (.add (RuntimePermission. "accessDeclaredMembers"))))

(defn domain [perms]
     (java.security.ProtectionDomain.
       (java.security.CodeSource. nil
                                  (cast java.security.cert.Certificate nil))
       perms))

(defn context [dom]
      (java.security.AccessControlContext. (into-array [dom])))

(defn priv-action [thunk]
      (proxy [java.security.PrivilegedAction] [] (run [] (thunk))))

(defn sandbox [thunk context]
      (java.security.AccessController/doPrivileged
        (priv-action thunk)
        context))

(defn write-test []
      (doto (-> "foo.txt" File. FileWriter.) (.write "foo") .close))

(defn de-fang
      "looks through the macroexpand of a form for things I don't allow"
      [form notallowed]
      (if (list? form)
          (when (not (some notallowed
                           (tree-seq seq?
                                     #(let [a (macroexpand %)] (or (and (seq? a) a) (list a)))
                                     form)))
              form)
          form))

(defn cond-eval [pred form]
      (if (pred form)
        (eval form)
        (throw (java.lang.Exception. "DENIED"))))

;(enable-security-manager) ; This doesn't need to be enabled by default

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

(defmacro my-doc [s]
  `(let [m# (meta (resolve '~s))
         al# (:arglists m#)
         docstring# (:doc m#)]
     (if m#
       (.replaceAll (str al# "; " docstring# ) "\\s+" " ")
       (-> hiredman.clojurebot.code-lookup/contrib
         :vars ((partial filter (fn [a#] (= (:name a#) (.toString '~s))))) first
         ((fn [foo#]
            (if foo#
              (.replaceAll (str (:namespace foo#) "/" (:name foo#) ";"  (print-str (:arglists foo#)) "; " (:doc foo#)) "\\s+" " ")
              (symbol (core/befuddled)))))))))

(defn force-lazy-seq
      "if passed a lazy seq, forces seq with doall, if not return what is passed"
      [s]
      (or (and (instance? clojure.lang.LazySeq s) (doall s)) s))

(defn eval-in-box-helper [form]
      (let [result (cond-eval #(de-fang % *bad-forms*) form)]
                          (.close *out*)
                          (.close *err*)
                          (let [o (str *out*)
                                e (str *err*)
                                r (prn-str (force-lazy-seq result))]
                            [o e (when (or result (.equals o "")) r)])))

(defn eval-in-box [_string sb-ns]
  (println _string)
      (let [form #(-> _string StringReader. PushbackReader. read)
            thunk (fn []
                      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)
                                 *ns* (find-ns sb-ns) doc (var my-doc) *print-level* 30]
                        (eval-in-box-helper (form))))
            result (thunk-timeout #(sandbox (fn [] (wrap-exceptions thunk))
                                            (context (domain (empty-perms-list)))) *default-timeout*)]
        result))
