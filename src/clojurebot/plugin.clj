(ns clojurebot.plugin
  (:use [clojure.java.io :only [file]])
  (:import [java.net URLClassLoader]
           [clojure.lang Compiler]))

(defn load-from [directory namespaces]
  (try
    (push-thread-bindings {Compiler/LOADER (URLClassLoader.
                                            (into-array
                                             (map
                                              #(.toURl
                                                (file-seq
                                                 (file directory))))))})
    (doseq [namespace namespaces]
      (require namespace))
    (finally
     (pop-thread-bindings))))
