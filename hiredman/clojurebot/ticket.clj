(ns hiredman.clojurebot.ticket
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util])
  (:import (java.io StringReader StringBufferInputStream)))

(def url "http://user:password@www.assembla.com/spaces/clojure/tickets/")

(def ticket #"^ticket #(\d)")

(defn get-ticket [n]
  (-> url (str n) util/get-url StringBufferInputStream. clojure.xml/parse))
;(def get-ticket (memoize get-ticket))

(defn ticket-nth [n]
  (-> n get-ticket :content ((partial filter #(#{:created-on :summary :status :priority} (:tag %))))
    ((partial reduce #(assoc % (:tag %2) (first (:content %2))) {}))
    (doto :prn)
    (update-in [:status] {"1" :accepted "0" :new "2" :invalid "3" :fixed "4" :test})
    (update-in [:priority] {"3" :normal "1" :highest "2" :high "4" :low "5" :lowest})
    (update-in [:summary] (fn [s] (.replaceAll s "\\s" " ")))))

(core/defresponder ::ticket-n 0
  (core/dfn (and (re-find ticket (core/extract-message bot msg))
                 (:addressed? (meta msg)))) ;;
  (let [m (core/extract-message bot msg)
        n (.replaceAll m (.toString ticket) "$1")]
    (core/new-send-out bot :msg msg (str (prn-str (assoc (ticket-nth n) :url (symbol (util/tinyurl (str url n)))))))))

(core/remove-dispatch-hook ::ticket-n)
