(ns hiredman.clojurebot.stock-quote
      (:require [hiredman.clojurebot.core :as core]
                [hiredman.utilities :as util]
                [org.danlarkin.json :as json]))

(def url "http://www.google.com/finance/info?q=")

(defn stock-quote [symbol]
  (json/decode-from-str (apply str (butlast (drop 4 (util/get-url (.concat url symbol)))))))

(defn format-quote [{:keys [l t c cp]}]
  (format "%s; %s" t cp))

(core/defresponder ::stock-quote 0
  (core/dfn (and (:addressed? (meta msg))
                 (re-find #"ticker [A-Z]+" (core/extract-message bot msg)))) ;;
  (core/send-out :msg bot msg (format-quote (stock-quote (.replaceAll (core/extract-message bot msg) "^ticker " "")))))

(core/remove-dispatch-hook ::stock-quote)
