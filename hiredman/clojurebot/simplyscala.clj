(ns hiredman.clojurebot.simplyscala
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.utilities :as util])
  (:import (java.net URLEncoder )))


(def scala-the-easy-way "http://www.simplyscala.com/interp?bot=irc&code=")

(defn scala-eval [code]
    (-> code URLEncoder/encode ((partial str scala-the-easy-way))
          util/get-url (.split "res0: ") rest ((partial apply str))))

(core/defresponder ::scala 0
                   (core/dfn (.startsWith (core/extract-message bot msg) ">"))
                   (core/new-send-out bot :msg msg (scala-eval (.substring (core/extract-message bot msg) 1))))

;(core/remove-dispatch-hook ::scala)
