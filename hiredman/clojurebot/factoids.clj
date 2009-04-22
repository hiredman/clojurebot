(ns hiredman.clojurebot.factoids
    (:require [hiredman.clojurebot.core :as core]
              [name.choi.joshua.fnparse :as fp]))

(defmacro string [str] (cons 'fp/conc (map #(list 'fp/lit %) str)))

(def literal (string "literal"))
(def spaces (fp/semantics (fp/rep* (fp/lit \space)) first))
(def number (fp/semantics (fp/rep+ (fp/term (set (map (comp first str) (range 10))))) #(Integer/parseInt (apply str %))))
(def character (fp/term #(instance? Character %)))
(def text (fp/rep+ character))
(def term (fp/rep+ (fp/except character (string " is "))))

(def definition (fp/semantics (fp/conc term (string " is ") text)
                              (fn [[term _ defi]] (vary-meta {:term (.trim (apply str term)) :definition (.trim (apply str defi))} assoc :type :def))))

(def definition-add
  (fp/semantics (fp/conc term (string " is ") (string "also") (fp/lit \space) text)
                              (fn [[term _ _ _ defi]] (vary-meta {:term (apply str term) :definition (apply str defi)} assoc :type :def-add))))

(def indexed-lookup (fp/semantics (fp/conc literal spaces (fp/lit \[) number (fp/lit \]) spaces (fp/semantics text (partial apply str))) (fn [[_ _ _ number _ _ term]] (vary-meta {:number number :term term} assoc :type :indexed-look-up))))
(def index-count (fp/semantics (fp/conc literal spaces (fp/lit \[) (fp/lit \?) (fp/lit \]) spaces (fp/semantics text (partial apply str))) (fn [[_ _ _ number _ _ term]] (vary-meta {:term term} assoc :type :count))))
(def index (fp/alt index-count indexed-lookup))

(def g (fp/alt index-count indexed-lookup definition-add definition))

(defn simple-lookup [term]
  (@core/dict-is term))

(defmulti f type)

(defmethod f :count [bag]
  (let [defi  (simple-lookup (:term bag))]
    (core/send-out :msg (:bot (meta bag)) (:message (meta bag))
                   (cond
                     (nil? defi)
                      0
                     (vector? defi)
                      (count defi)
                     :else
                      1))))

(defmethod f :indexed-look-up [bag]
  (let [defi (simple-lookup (:term bag))]
    (core/send-out :msg (:bot (meta bag)) (:message (meta bag))
                   (cond
                     (nil? defi)
                      "nothing defined"
                     (and (vector? defi) (> (count defi) (:number bag)))
                      (defi (:number bag))
                     (vector? defi)
                      (str (:number bag) " is out of range")
                     (zero? (:number bag))
                      defi
                     :else
                      (core/befuddled)))))

(core/defresponder ::literal 0
  (core/dfn (and (:addressed? (meta msg))
                 (index (core/extract-message bot msg) nil)))
  (f (vary-meta (first (index (core/extract-message bot msg) nil)) assoc :bot bot :message msg)))

(core/remove-dispatch-hook ::literal)
