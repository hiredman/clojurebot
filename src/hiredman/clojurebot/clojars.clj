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


(defn task [bot chan]
  (let [r (get-recent)
        new (difference r @recent)]
    (when (not (empty? new))
      (core/new-send-out bot :notice
                        chan
                        (str "recently on clojars.org: "(pr-str new))))
    (dosync (ref-set recent r))))

(defn go [bot]
  (sched/fixedrate
    {:name ::clojars
     :task #(task bot "#clojure")
     :start-delay 0
     :rate 30
     :unit (:minutes sched/unit)}))
