(ns clojurebot.infer
  (:require [clojure.core.logic :as logic]
            [clojure.core.logic.protocols :as lp]
            [clojurebot.triples :as trip]))

(declare db)

(defn infer [s p o subject predicate object level]
  (logic/fresh []
    (logic/pred level (partial > 3))
    (logic/conde
     ;; walk down the chain
     [(logic/fresh [s' p' o']
        (logic/== s' o)
        (db s' p' o')
        (logic/conde
         [(logic/== object o')]
         [(infer s' p' o' subject predicate object (inc level))]))]
     ;; walk up the chain
     [(logic/fresh [s' p' o']
        (logic/== o' o)
        (logic/!= s' subject)
        (db s' p' o')
        (logic/conde
         [(logic/== object s')]
         ;; new "subject", so re-query the db and recurse
         [(logic/fresh [s'' p'' o'']
            (logic/== s' s'')
            (db s'' p'' o'')
            (logic/conde
             [(logic/== object o'')]
             [(infer s'' p'' o'' subject predicate object (inc level))]))]))])))

(defn respondo [input]
  (distinct
   (logic/run* [q]
     (logic/fresh [subject predicate object inferred p o]
       (logic/== q {:subject subject
                    :predicate predicate
                    :object object
                    :infered? inferred})
       (logic/!= subject object)
       (db input p o)
       (logic/== subject input)
       (logic/== predicate p)
       (logic/conde
        [(logic/== [subject predicate object inferred]
                   [input p o false])]
        [(logic/== inferred true)
         (infer input p o subject predicate object 0)])))))

(defn db* [subject predicate object]
  (fn [a]
    (logic/to-stream
     (for [rec (let [s (lp/walk a subject)
                     s (if (logic/lvar? s)
                         :s
                         s)
                     p (lp/walk a predicate)
                     p (if (logic/lvar? p)
                         :p
                         p)
                     o (lp/walk a object)
                     o (if (logic/lvar? o)
                         :o
                         o)]
                 (trip/query s p o))]
       (trampoline
        (logic/fresh []
          (logic/== (:subject rec) subject)
          (logic/== (:predicate rec) predicate)
          (logic/== (:object rec) object))
        a)))))

(def db
  (logic/tabled [subject predicate object]
                (db* subject predicate object)))
