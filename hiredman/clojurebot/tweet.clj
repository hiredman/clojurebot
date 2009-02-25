(ns hiredman.clojurebot.tweet
    (:require [hiredman.clojurebot.core :as core])
    (:use (hiredman horizon))
    (:import (java.util.concurrent TimeUnit)
             (java.util Date)))

(def urlx "http://search.twitter.com/search.atom?q=")

(def nex (atom {}))

(def next-url
     (comp :href
           :attrs
           first
           (partial filter #(and (= (:rel (:attrs %)) "refresh" ) (= :link (:tag %))))
           :content))

(def xml
     (comp clojure.xml/parse
           #(.getContent %)
           #(java.net.URL. %)))

(defn url [term]
      (if-let [u (@nex term)]
              u
              (str urlx term)))

(defn interesting-tag? [x]
      (some #(= (:tag x) %) [:title :author]))

(def entries
     (comp (partial filter #(= :entry (:tag %)))
           :content))

(defn entry [t]
      (let [[content author] (map (comp first :content) (filter interesting-tag? (:content t)))
            author (first (re-find #"([^ ]+)" (first (:content author))))]
        [content author]))

(defn get-tweets [term]
      (let [xml (xml (url term))]
        (swap! nex #(assoc % term (next-url xml)))
        (seq (map entry (entries xml)))))

(defn get-latest-tweets [bot term channel]
      (core/send-out :notice bot channel "Tweets:"))
      (doseq [t (take 3 (get-tweets "clojure"))]
             (core/send-out :notice bot channel (str (first t) " --" (second t)))))

(defn watch [bot term channel]
      (.scheduleAtFixedRate core/task-runner
                            #(get-latest-tweets bot term channel)
                            (long 0)
                            (long 60)
                            TimeUnit/MINUTES))
