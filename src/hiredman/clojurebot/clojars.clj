(ns hiredman.clojurebot.clojars
  (:use [hiredman.utilities :only (get-url)]
        [clojure.set :only (difference)])
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util]
            [hiredman.schedule :as sched]))

(def recent (ref #{}))

(defn startparse-tagsoup [s ch]
      (let [p (org.ccil.cowan.tagsoup.Parser.)]
                    (.setContentHandler p ch)
                    (.parse p s)))

(defn zip-soup [url]
      (clojure.zip/xml-zip (clojure.xml/parse url startparse-tagsoup)))

(defn get-recent []
  (-> "http://clojars.org" zip-soup first
    ((partial tree-seq map? (comp seq :content)))
    ((partial filter #(= :ul (:tag %)))) last :content
    ((partial map (comp first :content first :content))) set))


(defn go []
  (let [r (get-recent)
        new (difference r @recent)]
    (dosync (ref-set recent r))
    (when (not (empty? new))
      (str "recently on clojars.org: " (pr-str new)))))
