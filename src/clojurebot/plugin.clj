(ns clojurebot.plugin
  (:use [clojure.java.io :only [file]])
  (:import [java.net URLClassLoader]
           [clojure.lang Compiler]))

(defn load-from [directory namespaces]
  (push-thread-bindings {Compiler/LOADER (URLClassLoader.
                                          (into-array
                                           (map
                                            #(.toURL %)
                                            (file-seq (file directory)))))})
  (try
    (doseq [namespace (set namespaces)]
      (try
        (require namespace)
        (catch Exception e
          (.printStackTrace e))))
    (finally
     (pop-thread-bindings))))
