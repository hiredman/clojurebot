;; posts lines containing urls to a delicious account
(ns hiredman.clojurebot.delicious
  (:use [hiredman.clojurebot.core :only (defresponder2)])
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util])
  (:import (java.net URLEncoder URL)))

(def url-reg #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

(def pastebins
  #"(fpaste\.org|dpaste\.*|gist\.github\.com|paste.lisp.org/display|.*pastebin\.com|p\.hagelb\.org|pastebin\.org|paste\.pocoo\.org/show|pastie\.org|sprunge\.us)")

;;#"(\w+://.*?)[.>]*(?: |$)"

(defn post
  "posts a url to the delicious account of [user pass]"
  [[user pass] url descr tag]
  (util/shell (str "fetch -o /dev/null https://" user ":" pass "@api.del.icio.us/v1/posts/add?url=" (URLEncoder/encode url) "&description=" (URLEncoder/encode descr) "&tags=" (URLEncoder/encode tag))))

;;(core/defresponder ::delicious 21
;;  (core/dfn (and (re-find url-reg (:message msg))
;;                 (:channel msg))) ;;
;;  (let [url (re-find url-reg (:message msg))
;;        desc (:message msg)
;;        tag (str (:sender msg) " " (:channel msg)
;;                 (when (re-find #"lisppaste" (:sender msg)) (str " " (first (.split desc " ")))))
;;        tag (if (re-find pastebins url)
;;              (str tag " pastbin")
;;              tag)]
;;    (post (:delicious bot) url desc tag)))

(defresponder2
  {:name ::delicious
   :priority 21
   :dispatch (fn [bot msg]
               (and (re-find url-reg (:message msg))
                    (:channel msg)))
   :body (fn [bot msg]
           (let [url (re-find url-reg (:message msg))
                 desc (:message msg)
                 tag (str (:sender msg) " " (:channel msg)
                          (when (re-find #"lisppaste" (:sender msg)) (str " " (first (.split desc " ")))))
                 tag (if (re-find pastebins url)
                      (str tag " pastbin")
                      tag)]
             (post (:delicious bot) url desc tag)))})
