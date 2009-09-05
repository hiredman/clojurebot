(ns hiredman.clojurebot.factoids
  (:require [hiredman.clojurebot.core :as core]
            [name.choi.joshua.fnparse :as fp]))

(defmacro string [str] (cons 'fp/conc (map #(list 'fp/lit %) str)))

(def literal (string "literal")) ;literally the string "literal"
(def spaces (fp/semantics (fp/rep* (fp/lit \space)) first)) ;collapses spaces
(def number (fp/semantics (fp/rep+ (fp/term (set (map (comp first str) (range 10))))) #(Integer/parseInt (apply str %))))
(def character (fp/term #(instance? Character %))) ;)any character

(def text (fp/rep+ (fp/except character (fp/lit \?))))

;(def escaped-is (fp/followed-by (fp/lit (char 92)) (string "is"))) ;\is

(def escaped-is (fp/conc (fp/lit (char 92)) (string "is")))

(def term (fp/rep+ (fp/except character (fp/except (string " is ") escaped-is)))) ;a bunch of characters up to the first not escaped is

(def definition
  (fp/semantics (fp/conc term (string " is ") text)
                (fn [[term _ defi]]
                  (vary-meta {:term (.trim (apply str term)) :definition (.trim (apply str defi))}
                             assoc :type :def))))

(def definition-add (fp/semantics (fp/conc term (string " is ") (string "also") (fp/lit \space) text) (fn [[term _ _ _ defi]] (vary-meta {:term (apply str term) :definition (apply str defi)} assoc :type :def-add))))
(def indexed-lookup (fp/semantics (fp/conc literal spaces (fp/lit \[) number (fp/lit \]) spaces (fp/semantics text (partial apply str))) (fn [[_ _ _ number _ _ term]] (vary-meta {:number number :term term} assoc :type :indexed-look-up))))
(def index-count (fp/semantics (fp/conc literal spaces (fp/lit \[) (fp/lit \?) (fp/lit \]) spaces (fp/semantics text (partial apply str))) (fn [[_ _ _ number _ _ term]] (vary-meta {:term term} assoc :type :count))))
(def index (fp/alt index-count indexed-lookup))
(def factoid-command (fp/alt index-count indexed-lookup definition-add definition))

(defn simple-lookup [term]
  (@core/dict-is term))

(defmulti factoid-command-processor type :default :boom)

(defmethod factoid-command-processor :boom [bag]
  (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag)) (core/befuddled)))

(defmethod factoid-command-processor :count [bag]
  (let [defi  (simple-lookup (:term bag))]
    (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag))
                   (cond
                     (nil? defi)
                        0
                     (vector? defi)
                        (count defi)
                     :else
                        1))))

(defmethod factoid-command-processor :indexed-look-up [bag]
  (let [defi (simple-lookup (:term bag))]
    (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag))
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

(defmethod factoid-command-processor :def [bag]
  (core/is- (:bot (meta bag)) (:term bag) (:definition bag))
  (core/is!  (:term bag) (:definition bag))
  (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag)) (core/ok)))

(defmethod factoid-command-processor :def-add [bag]
  (core/is- (:bot (meta bag)) (:term bag) (str "also " (:definition bag)))
  (core/is (:term bag) (:definition bag))
  (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag)) (core/ok)))

(core/defresponder ::factoids 0
  (core/dfn (and (:addressed? (meta msg)) (factoid-command {:remainder (seq (core/extract-message bot msg))})))
  (factoid-command-processor (vary-meta (first (doto (factoid-command {:remainder (seq (core/extract-message bot msg) )}) prn)) assoc :bot bot :message msg)))

;(core/remove-dispatch-hook ::factoids)
