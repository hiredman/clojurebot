(ns hiredman.clojurebot.github
 (:require [hiredman.clojurebot.core :as core]
           [org.danlarkin.json :as json]
           [hiredman.utilities :as util]))

(def api-url "http://github.com/api/v2/json/commits/list/richhickey/clojure/master")

(def commit-url "http://github.com/api/v2/json/commits/show/richhickey/clojure")

(defn get-commit [n]
  (-> commit-url (str "/" n) util/get-url json/decode-from-str :commit :message))

(core/defresponder ::git-commit 0
  (core/dfn (and (re-find #"^git ([^ ])" (core/extract-message bot msg))
                 (:addressed? (meta msg)))) ;;
  (let [m (.replaceAll (core/extract-message bot msg) (.toString #"^git ([^ ])") "$1")]
    (core/new-send-out bot :msg msg (get-commit m))))

(core/remove-dispatch-hook ::git-commit)

;(-> x :commits first :committed_date (.replaceAll "([+-])(\\d\\d):(\\d\\d)" "$1$2$3") (util/date "yyyy-MM-dd'T'HH:mm:ssZ"))
;(def x (json/decode-from-str (util/get-url api-url)))

