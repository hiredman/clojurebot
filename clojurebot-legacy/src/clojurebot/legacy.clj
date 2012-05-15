(ns clojurebot.legacy
  (:require [vespa.crabro :as vc]
            [vespa.protocols :as vp])
  (:gen-class))

(defn -main [& args]
  (with-open [mb (vc/message-bus)]
    (while true
      (try
        (vp/receive-from
         mb
         "fnparse"
         (fn [msg]
           (let [{:keys [payload reply-to]} (read-string msg)]
             (vc/send-to mb
                         reply-to
                         (pr-str
                          (try
                            {:good (eval payload)}
                            (catch Throwable t
                              {:error (print-str t)})))))))
        (catch Throwable t
          (prn t))))))
