(ns hiredman.clojurebot.edit
    (:require [hiredman.clojurebot.core :as core]))

(core/defresponder ::edit 0
  (core/dfn (and (:addressed? (meta msg))
                 (re-find #"^.* = s/.*/.*/" (core/extract-message bot msg)))) ;;
  (let [m (core/extract-message bot msg)
        [_ original replacement] (.split (re-find #"s/.*/.*/" m) "/")]
   (prn [original replacement])))

(core/remove-dispatch-hook ::edit)
