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
    (doseq [namespace namespaces]
      (require namespace))
    (finally
     (pop-thread-bindings))))
