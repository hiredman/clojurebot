(ns hiredman.clojurebot.simplyscala
  (:use [hiredman.clojurebot.core :only (defresponder2 new-send-out extract-message)])
  (:require [hiredman.utilities :as util])
  (:import (java.net URLEncoder )))


(def scala-the-easy-way "http://www.simplyscala.com/interp?bot=irc&code=")

(defn scala-eval [code]
    (-> code URLEncoder/encode ((partial str scala-the-easy-way))
          util/get-url (.split "res0: ") rest ((partial apply str))))

(defresponder2
  {:priority 0
   :name ::scala
   :dispatch (fn [bot msg]
               (and (:addressed? (meta msg))
                    (.startsWith (extract-message bot msg) "scala")))
   :body (fn [bot msg]
           (new-send-out bot :msg msg
             (scala-eval
               (let [m (extract-message bot msg)]
                 (if (.startsWith m "scala")
                   (.replaceAll m "^scala" "")
                   (.substring m 1))))))})
