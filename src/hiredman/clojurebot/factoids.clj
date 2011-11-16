(ns hiredman.clojurebot.factoids
  (:use [hiredman.clojurebot.storage :only (db-name)])
  (:require [hiredman.clojurebot.core :as core]
            [hiredman.triples :as trip]
            [name.choi.joshua.fnparse :as fp]
            [opennlp.nlp :as nlp]))

;;BEGIN GARBAGE
(defmacro string [str] (cons 'fp/conc (map #(list 'fp/lit %) str)))

(def literal (string "literal")) ;literally the string "literal"

(def spaces (fp/semantics (fp/rep* (fp/lit \space)) first)) ;collapses spaces

(def number
  (fp/semantics
   (fp/rep+
    (fp/term (set (map (comp first str) (range 10)))))
   #(Integer/parseInt (apply str %))))

(def character (fp/term #(instance? Character %))) ;any character

(def text (fp/rep+ (fp/except
                    (fp/alt character
                            (fp/conc character
                                     (fp/lit \?)
                                     character))
                    (fp/lit \?))))

;;(def escaped-is (fp/followed-by (fp/lit (char 92)) (string "is"))) ;\is

(def escaped-is (fp/conc (fp/lit (char 92)) (string "is")))

(def term
  (fp/rep+ (fp/except character (fp/except (string " is ") escaped-is))))
;;a bunch of characters up to the first not escaped is

(def definition
  (fp/semantics
   (fp/conc term (string " is ") text)
   (fn [[term _ defi]]
     (vary-meta {:term (.trim (apply str term))
                 :definition (.trim (apply str defi))}
                assoc :type :def))))

(def definition-add
  (fp/semantics
   (fp/conc term (string " is ") (string "also") (fp/lit \space) text)
   (fn [[term _ _ _ defi]]
     (vary-meta {:term (apply str term)
                 :definition (apply str defi)} assoc :type :def))))

(def indexed-lookup
  (fp/semantics
   (fp/conc literal
            spaces
            (fp/lit \[)
            number
            (fp/lit \])
            spaces
            (fp/semantics text (partial apply str)))
   (fn [[_ _ _ number _ _ term]]
     (vary-meta {:number number :term term} assoc :type :indexed-look-up))))

(def index-count
  (fp/semantics
   (fp/conc literal
            spaces
            (fp/lit \[)
            (fp/lit \?)
            (fp/lit \])
            spaces
            (fp/semantics text (partial apply str)))
   (fn [[_ _ _ number _ _ term]]
     (vary-meta {:term term} assoc :type :count))))

(def index (fp/alt index-count indexed-lookup))

(def predicate
  (fp/semantics
   (fp/conc (fp/lit \|)
            (fp/rep+ (fp/except character (fp/lit \|))) (fp/conc (fp/lit \|)))
   (fn [[_ pred _]] (.trim (apply str pred)))))

(def subject (fp/semantics (fp/rep+ (fp/except character (fp/lit \|)))
                           (fn [d] (.trim (apply str d)))))

(def object (fp/semantics (fp/rep+ character)
                          (fn [o] (.trim (apply str o)))))

(def predicate-style-definition
  (fp/semantics (fp/conc subject predicate object)
                (fn [[subject predicate object]]
                  #^{:type :predicate-style-definition}
                  {:subject subject :object object :predicate predicate})))

(def forget
  (fp/semantics (fp/conc (string "forget ") predicate-style-definition)
                (fn [[_ o]]
                  (with-meta o {:type :forget}))))

;;END GARBAGE

;;parse a string into some kind of factoid related something or other
;;takes arguments in the style of fnparse {:remainder (seq some-string)}
(def factoid-command
  (fp/alt index-count
          indexed-lookup
          forget
          definition-add
          definition
          predicate-style-definition))

;;this should be ditched
(defn simple-lookup [term]
  (@core/dict-is term))

(defmulti factoid-command-processor (comp type second list) :default :boom)

(defmethod factoid-command-processor :boom [config bag]
  (core/befuddled))

(defmethod factoid-command-processor :count [config bag]
  (let [defi  (simple-lookup (:term bag))]
    (cond
     (nil? defi)
     0
     (vector? defi)
     (count defi)
     :else
     1)))

;;this too
(defmethod factoid-command-processor :indexed-look-up [config bag]
  (let [defi (simple-lookup (:term bag))]
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
     (core/befuddled))))

(defmethod factoid-command-processor :def [config bag]
  (trip/store-triple
   (trip/derby (db-name config))
   {:s (:term bag) :o (:definition bag) :p "is"})
  (core/ok))

(defmethod factoid-command-processor :predicate-style-definition [config bag]
  (trip/store-triple
   (trip/derby (db-name config))
   {:s (:subject bag) :o (:object bag) :p (:predicate bag)})
  (core/ok))

(defmethod factoid-command-processor :forget [config bag]
  (prn :forget)
  (trip/delete
   (trip/derby (db-name config)) (:subject bag) (:predicate bag) (:object bag))
  (format "I forgot that %s %s %s"
          (:subject bag)
          (:predicate bag)
          (:object bag)))

;;(defmethod factoid-command-processor :def-add [bag]
;;  (trip/store-triple (trip/derby (db-name (:bot (meta bag)))) {:s (:term bag) :o (:definition bag) :p "is"})
;;  (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag)) (core/ok)))


(defn factoid-command? [message]
  (and (not (.endsWith message "?"))
       (factoid-command {:remainder (seq message)})))

(defn factoid-command-run [{:keys [config message]}]
  (println "@factoid-command-run" config message)
  (factoid-command-processor
   config
   (first
    (factoid-command
     {:remainder (seq message)}))))


;;(core/remove-dispatch-hook ::factoids)
;;(hiredman.triples/import-file
;; (hiredman.triples/derby (db-name bot)) (str
;; (hiredman.clojurebot.core/dict-file bot ".is")))

(defn replace-with [str map]
  (reduce #(.replaceAll % (first %2) (second %2)) str map))

(defn prep-reply
  "preps a reply, does substituion of stuff like <reply> and #who"
  [sender term pred defi bot]
  (replace-with
   (if (or (re-find #"^<reply>" defi)
           (re-find #"^<REPLY>" defi))
     (.trim
      (core/remove-from-beginning
       (core/remove-from-beginning (str defi) "<reply>")
       "<REPLY>"))
     (format "%s %s %s" term pred defi))
   {"#who" sender "#someone" (core/random-person bot)}))


(defmulti #^{:doc "" :private true}
  befuddled-or-pick-random (comp empty? first list))

(defmethod befuddled-or-pick-random false [x bag]
  (-> x
      ((fn [x] (x (rand-int (count x)))))
      ((fn [{:keys [subject object predicate]}]
         (prep-reply (:sender bag)
                     subject
                     predicate
                     object
                     (:bot bag))))))

(defmethod befuddled-or-pick-random true [x bag] (core/befuddled))

(defn mutli-query [config pos form]
  (with-meta ((partial mapcat
                       #(trip/query
                         (trip/derby (db-name config))
                         (list (format form %)) :z :y)) pos)
    (meta pos)))

(def get-sentences (delay (nlp/make-sentence-detector "EnglishSD.bin.gz")))
(def tokenize (delay (nlp/make-tokenizer "EnglishTok.bin.gz")))
(def pos-tag (delay (nlp/make-pos-tagger "tag.bin.gz")))

(defn tag [x]
  (@pos-tag
   (@tokenize x)))

(def noun-filter
  (comp
   (partial map first)
   (partial filter #(.startsWith (second %) "N"))))

(defn qw [input config]
  (let [first-order
        (if-let [result (seq (trip/query (trip/derby (db-name config))
                                         input :y :z))]
          result
          (-> input
              tag
              noun-filter
              (#(mutli-query config % "%%%s%%"))))]
    (try
      (letfn [(infer [order result]
                (when (and (= "is" (:predicate result))
                           (> 3 order))
                  (lazy-seq
                   (cons result
                         (->> (trip/query (trip/derby (db-name config))
                                          (:object result)
                                           "is"
                                           :z)
                              (mapcat (partial f (inc order)))
                              (map (fn [x]
                                     (assoc x
                                       :subject
                                       (:subject result)))))))))]
        (apply concat
               (filter identity
                       (pmap (partial infer 1)
                             (shuffle first-order)))))
      (catch Exception e
        (println e)))
    #_first-order))

(defn factoid-lookup [{:keys [message config] :as bag}]
  (-> (.replaceAll (.trim message) "\\?$" "")
      (qw config)
      (doto prn)
      vec
      (befuddled-or-pick-random bag)))

(defn factoid-lookup-no-fall-back [{:keys [message config] :as bag}]
  (let [x (-> (.replaceAll (.trim message) "\\?$" "")
              (qw config)
              vec)]
    (if (empty? x)
      nil
      (let [{:keys [subject predicate object]} (first (shuffle x))]
        (prep-reply (:sender bag)
                    subject
                    predicate
                    object
                    (:bot bag))))))
