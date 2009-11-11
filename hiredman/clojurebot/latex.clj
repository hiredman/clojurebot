(ns hiredman.clojurebot.latex
  (:use [hiredman.clojurebot.core :only (defresponder2 extract-message new-send-out)]
        [hiredman.utilities :only (tinyurl)])
  (:import (java.net URLEncoder)))

(defn chart-url [latex]
  (format "http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFFFF&chco=000000&chl=%s"
          (URLEncoder/encode latex)))

(defresponder2
  {:name ::latex
   :priority 0
   :dispatch (fn [bot msg]
               (re-find #"^latex " (extract-message bot msg)))
   :body (fn [bot msg]
           (let [m (.replaceAll (extract-message bot msg)
                                "^latex "
                                "")]
             (new-send-out bot :notice msg (tinyurl (chart-url m)))))})
