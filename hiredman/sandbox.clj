(ns hiredman.sandbox
    (:import (java.util.concurrent FutureTask TimeUnit TimeoutException)
             (java.io File FileWriter PushbackReader StringReader)))

;(def *bad-forms* #{'eval 'catch 'try 'def 'defn 'defmacro 'read 'Thread. 'send 'send-off 'clojure.asm.ClassWriter.})

(def *bad-forms* #{'clojure.core/defmacro 'clojure.core/defn 'def 'eval 'try 'catch 'send})

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
                           (println (.getMessage %)) 
                           (.getMessage %))))
  ([f exception-handler]
     (try (f) (catch Exception e (exception-handler e)))))
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

(defn de-fang
      "looks through the macroexpand of a form for things I don't allow"
      [form notallowed]
      (if (list? form)
          (when (not
                  (some notallowed
                        (tree-seq seq? identity (macroexpand form))))
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

(defn eval-in-box [_string sb-ns]
      (let [form (-> _string StringReader. PushbackReader. read)
            thunk (fn []
                      (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)
                                 *ns* (find-ns sb-ns)]
                        (let [result (cond-eval #(de-fang % *bad-forms*) form)]
                          (.close *out*)
                          (.close *err*)
                          [(str *out*) (str *err*) (binding [*out* (java.io.StringWriter.)]
                                                            (prn result)
                                                            (.close *out*)
                                                            (str *out*))])))
            result (thunk-timeout #(sandbox (fn [] (wrap-exceptions thunk))
                                            (context (domain (empty-perms-list)))) *default-timeout*)]
        result))
