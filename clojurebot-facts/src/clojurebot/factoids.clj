(ns clojurebot.factoids
  (:require [clojurebot.triples :as trip]
            [clojure.java.io :as io]
            [name.choi.joshua.fnparse :as fp]
            [opennlp.nlp :as nlp]
            [clj-http.client :as http]
            [clojure.tools.logging :as log]
            [clojurebot.infer]))

(def ^:dynamic *id*)

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
  #_(@core/dict-is term))

(defmulti factoid-command-processor (comp type second list) :default :boom)

(defn befuddled []
  (let [{:keys [body]} (http/get "http://localhost:3205/befuddled")]
    (read-string body)))

(defmethod factoid-command-processor :boom [_ bag]
  (befuddled))

(defmethod factoid-command-processor :count [_ bag]
  (let [defi  (simple-lookup (:term bag))]
    (cond
     (nil? defi)
     0
     (vector? defi)
     (count defi)
     :else
     1)))

;;this too
(defmethod factoid-command-processor :indexed-look-up [_ bag]
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
     (befuddled))))

(defmethod factoid-command-processor :def [_ bag]
  (trip/store-triple
   {:s (:term bag) :o (:definition bag) :p "is"})
  (let [{:keys [body]} (http/get "http://localhost:3205/ok")]
    (read-string body)))

(defmethod factoid-command-processor :predicate-style-definition [_ bag]
  (trip/store-triple
   {:s (:subject bag) :o (:object bag) :p (:predicate bag)})
  (let [{:keys [body]} (http/get "http://localhost:3205/ok")]
    (read-string body)))

(defmethod factoid-command-processor :forget [_ bag]
  (trip/delete (:subject bag) (:predicate bag) (:object bag))
  (format "I forgot that %s %s %s"
          (:subject bag)
          (:predicate bag)
          (:object bag)))

;;(defmethod factoid-command-processor :def-add [bag]
;;  (trip/store-triple (trip/derby (db-name (:bot (meta bag)))) {:s (:term bag) :o (:definition bag) :p "is"})
;;  (core/new-send-out (:bot (meta bag)) :msg (:message (meta bag)) (core/ok)))


(defn factoid-command? [{:keys [message]}]
  (and (not (.endsWith message "?"))
       (factoid-command {:remainder (seq message)})))

(defn factoid-command-run [{:keys [_ message]}]
  (println "@factoid-command-run" message)
  (factoid-command-processor
   _
   (first
    (factoid-command
     {:remainder (seq message)}))))


;;(core/remove-dispatch-hook ::factoids)
;;(hiredman.triples/import-file
;; (hiredman.triples/derby (db-name bot)) (str
;; (hiredman.clojurebot.core/dict-file bot ".is")))

(defn replace-with [str map]
  (reduce #(.replaceAll % (first %2) (second %2)) str map))

(defn remove-from-beginning
  "return a string with the concatenation of the given chunks removed if it is
   found at the start of the string"
  [string & chunks]
  (.replaceFirst string (apply str "^" chunks) ""))

(defn prep-reply
  "preps a reply, does substituion of stuff like <reply> and #who"
  [sender term pred defi bot]
  (let [{:keys [body]} (http/get (str "http://localhost:3205/randomperson/" *id*))]
    (replace-with
     (if (or (re-find #"^<reply>" defi)
             (re-find #"^<REPLY>" defi))
       (.trim
        (remove-from-beginning
         (remove-from-beginning (str defi) "<reply>")
         "<REPLY>"))
       (format "%s %s %s" term pred defi))
     {"#who" sender "#someone" (read-string body)})))


(defmulti #^{:doc "" :private true}
  befuddled-or-pick-random (comp empty? first list))

(def infered-results (atom ()))

(defmethod befuddled-or-pick-random false [x bag]
  (-> x
      ((fn [x]
         (let [r (x (rand-int (count x)))]
           (if (and (:infered? r)
                    (zero? (rand-int 2)))
             (recur x)
             r))))
      ((fn [{:keys [subject object predicate infered?] :as relationship}]
         (clojure.tools.logging/info "CHOSEN" relationship)
         (when infered?
           (swap! infered-results
                  (fn [db record]
                    (if (> (count db) 10)
                      (recur (rest db) record)
                      (conj db record)))
                  [(System/currentTimeMillis) (:channel bag) relationship]))
         (prep-reply (:sender bag)
                     subject
                     predicate
                     object
                     (:bot bag))))))

(defmethod befuddled-or-pick-random true [x bag]
  (log/info *id* x bag)
  (befuddled))

(defn mutli-query [_ pos form]
  (with-meta ((partial mapcat
                       #(trip/query
                         (list (format form %)) :z :y)) pos)
    (meta pos)))

(def get-sentences
  (delay
   (with-open [s (.openStream (io/resource "en-sent.bin"))]
     (nlp/make-sentence-detector s))))

(def tokenize
  (delay
   (with-open [s (.openStream (io/resource "en-token.bin"))]
     (nlp/make-tokenizer s))))

(def pos-tag
  (delay
   (with-open [s (.openStream (io/resource "en-pos-maxent.bin"))]
     (nlp/make-pos-tagger s))))

(defn tag [x]
  (@pos-tag
   (@tokenize x)))

(def noun-filter
  (comp
   (partial map first)
   (partial filter #(.startsWith (second %) "N"))))

(defn search-term [thing]
  (:object thing))

(defn be [word]
  (if (.endsWith word "s")
    "are"
    "is"))

(defmacro tl [n & body]
  `(let [now# (System/nanoTime)]
     (try
       ~@body
       (finally
         (clojure.tools.logging/info ~(str "Elapsed " n " time:")
                                     (/ (double (- (System/nanoTime) now#))
                                        1000000.0)
                                     "msecs")))))

(defn qw [input _]
  (tl
   :qw
   (->> (clojurebot.infer/respondo input)
        (group-by (juxt :subject :predicate :object))
        (map (fn [[k v]]
               (let [{known false infered true} (group-by :infered? v)]
                 (first (concat known infered))))))))

(defn factoid-lookup [{:keys [message] :as bag}]
  (-> (.replaceAll (.trim message) "\\?$" "")
      ((fn [thing]
         (when (= "botsnack" thing)
           (let [now (System/currentTimeMillis)]
             (doseq [[ts chan fact] @infered-results
                     :when (= chan (:channel bag))
                     :when (> (+ ts (* 1000 60 2))
                              now)]
               (future
                 (trip/store-triple
                  {:s (:subject fact)
                   :o (:object fact)
                   :p (:predicate fact)})
                 (swap!
                  infered-results
                  (partial remove
                           #(= (select-keys
                                % [:object :predicate :subject])
                               (select-keys
                                fact [:object :predicate :subject]))))))))
         thing))
      (qw nil)
      vec
      (befuddled-or-pick-random bag)))

(defn factoid-lookup-no-fall-back [{:keys [message] :as bag}]
  (let [x (-> (.replaceAll (.trim message) "\\?$" "")
              (qw nil)
              vec)]
    (if (empty? x)
      nil
      (let [{:keys [subject predicate object infered?] :as relationship}
            (first (shuffle x))]
        (clojure.tools.logging/info "CHOSEN" relationship)
        (when infered?
          (swap! infered-results
                 (fn [db record]
                   (if (> (count db) 10)
                     (recur (rest db) record)
                     (conj db record)))
                 [(System/currentTimeMillis) (:channel bag) relationship]))
        (prep-reply (:sender bag)
                    subject
                    predicate
                    object
                    (:bot bag))))))
