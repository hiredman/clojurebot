(ns hiredman.sandbox
    (:import (java.util.concurrent FutureTask TimeUnit TimeoutException)
             (java.io File FileWriter PushbackReader StringReader)))

(def *bad-forms* #{'eval 'catch 'try 'def 'defn 'defmacro 'read 'Thread. 'send 'send-off 'clojure.asm.ClassWriter.})

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
 
(defn wrap-exceptions [f]
        (try (f) (catch Exception e (str (.getMessage e)))))
;;;;;;;;;;;

(defn empty-perms-list []
      (java.security.Permissions.))

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

(defn de-fang [form notallowed]
      (if (list? form)
          (when (not (some #(contains? notallowed %) (tree-seq list? identity form))) form)
          form))

(defn cond-eval [pred form]
      (if (pred form)
        (eval form)
        (throw (java.lang.Exception. "DENIED"))))

(enable-security-manager)

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

(defn box [_string]
      (let [form (-> _string StringReader. PushbackReader. read)
            thunk (fn []
                      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)
                                 *ns* (find-ns 'foo)]
                        (let [result (cond-eval #(de-fang % *bad-forms*) form)]
                          (.close *out*)
                          (.close *err*)
                          [(str *out*) (str *err*) (str result)])))
            result (thunk-timeout #(sandbox (fn [] (wrap-exceptions thunk))
                                            (context (domain (empty-perms-list)))) 10)]
        result))
