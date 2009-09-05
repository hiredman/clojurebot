(ns hiredman.clojurebot.ticket
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util])
  (:import (java.io StringReader StringBufferInputStream)))

(def url "http://www.assembla.com/spaces/clojure/tickets/")
(def contrib-url "http://www.assembla.com/spaces/clojure-contrib/tickets/")
(def ready-to-test-url "http://www.assembla.com/spaces/clojure/tickets?tickets_report_id=4")

(def ticket #"^ticket #(\d)")
(def contrib-ticket #"^contrib ticket #(\d)")

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

(core/defresponder ::ticket-n 0
  (core/dfn (and (re-find ticket (core/extract-message bot msg))
                 (:addressed? (meta msg)))) ;;
  (let [m (core/extract-message bot msg)
        n (.replaceAll m (.toString ticket) "$1")]
    (core/new-send-out bot :msg msg (str (prn-str (assoc (ticket-nth n) :url (symbol (util/tinyurl (str url n)))))))))

(core/defresponder ::contrib-ticket-n 0
  (core/dfn (and (re-find contrib-ticket (core/extract-message bot msg))
                 (:addressed? (meta msg)))) ;;
  (let [m (core/extract-message bot msg)
        n (.replaceAll m (.toString contrib-ticket) "$1")]
    (core/new-send-out bot :msg msg (str (prn-str (assoc (binding [url contrib-url] (ticket-nth n)) :url (symbol (util/tinyurl (str contrib-url n)))))))))

;(core/remove-dispatch-hook ::contrib-ticket-n)
