(ns hiredman.clojurebot.delicious
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util])
  (:import (java.net URLEncoder URL)))

(def url-reg #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

(defn post [[user pass] url descr tag]
      (util/shell (str "fetch -o /dev/null https://" user ":" pass "@api.del.icio.us/v1/posts/add?url=" (URLEncoder/encode url) "&description=" (URLEncoder/encode descr) "&tags=" (URLEncoder/encode tag))))

;;(defmethod core/responder ::delicious [bot pojo]
;;  (let [url (re-find url-reg (:message pojo))
;;        desc (:message pojo)
;;        tag (str (:sender pojo) " " (:channel pojo))]
;;    (post (:delicious bot) url desc tag)))
;;
;;(core/add-dispatch-hook 20 (core/dfn (and (re-find url-reg (:message msg)) (:channel msg))) ::delicious)

(core/defresponder ::delicious 20
  (core/dfn (and (re-find url-reg (:message msg))
                 (:channel msg))) ;;
  (let [url (re-find url-reg (:message pojo))
        desc (:message pojo)
        tag (str (:sender pojo) " " (:channel pojo))]
    (post (:delicious bot) url desc tag)))
