(ns hiredman.clojurebot.ticket
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util]
            [clojure.xml]
            [clojure.zip])
  (:import (java.io StringReader StringBufferInputStream)
           (java.net URLEncoder)))

(def url "http://www.assembla.com/spaces/clojure/tickets/")
(def contrib-url "http://www.assembla.com/spaces/clojure-contrib/tickets/")
(def ready-to-test-url "http://www.assembla.com/spaces/clojure/tickets?tickets_report_id=4")

(def ticket #"^ticket #(\d)")
(def contrib-ticket #"^contrib ticket #(\d)")
(def search-tickets-pattern #"^ticket search (.*)")

(defn search [term]
  (format "http://www.assembla.com/spaces/clojure/search?q=%s&commit=Go&search[flows]=0&search[wiki]=0&search[tickets]=1&search[tickets]=0&search[documents]=0" (URLEncoder/encode term)))

(defn parse-str [str]
  (-> str StringBufferInputStream. clojure.xml/parse))

(defn get-ticket [n]
  (-> url (str n) util/get-url parse-str))
                                        ;(def get-ticket (memoize get-ticket))

(defn ticket-nth "get the nth ticket" [n]
  (-> n get-ticket :content ((partial filter #(#{:created-on :summary :status :priority} (:tag %))))
      ((partial reduce #(assoc % (:tag %2) (first (:content %2))) {}))
      (update-in [:status] {"1" :accepted "0" :new "2" :invalid "3" :fixed "4" :test})
      (update-in [:priority] {"3" :normal "1" :highest "2" :high "4" :low "5" :lowest})
      (update-in [:summary] (fn [s] (.replaceAll s "\\s" " ")))))

(defn ticket-query? [{:keys [message]}]
  (re-find ticket message))

(defn get-ticket-n [{:keys [message]}]
  (let [n (.replaceAll message (.toString ticket) "$1")]
    (pr-str (assoc (ticket-nth n) :url (symbol (util/tinyurl (str url n)))))))

(declare search-tickets-for)

(defn contrib-ticket-query? [{:keys [message]}]
  (re-find contrib-ticket message))

(defn get-contrib-ticket-n [{:keys [message]}]
  (let [n (.replaceAll message (.toString contrib-ticket) "$1")]
    (prn-str (assoc (binding [url contrib-url] (ticket-nth n)) :url (symbol (util/tinyurl (str contrib-url n)))))))

(defn startparse-tagsoup [s ch]
  (let [p (org.ccil.cowan.tagsoup.Parser.)]
    (.setContentHandler p ch)
    (.parse p s)))

(defn zip-soup [url]
  (clojure.zip/xml-zip (clojure.xml/parse url startparse-tagsoup)))

(defn search-tickets-for [term]
  (-> term search zip-soup first :content
      ((partial filter #(= :body (:tag %)))) first :content
      ((partial filter #(= :div (:tag %))))
      ((partial filter #(= "content" ((comp :id :attrs) %))))
      ((partial map :content)) first ((partial map :content))
      ((partial map first)) ((partial filter #(= :ul (:tag %)))) first :content
      ((partial map :content))
      ((partial map first))
      ((partial mapcat :content))
      ((partial filter #(= :h4 (:tag %))))
      ((partial mapcat :content))
      ((partial filter #(= :a (:tag %))))
      ((partial mapcat :content))))

(defn ticket-search? [{:keys [message]}]
  (println message)
  (re-find search-tickets-pattern message))

(defn search-tickets [{:keys [message]}]
  (prn-str
   (search-tickets-for
    (last (re-find search-tickets-pattern message)))))
