(ns clojurebot.json
  (:require [cheshire.core :as json]))

(defn decode-from-str [json]
  (json/decode json true))
