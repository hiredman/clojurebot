(ns clojurebot.infer
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.protocols :as lp]
            [clojurebot.triples :as trip]
            [clojure.core.logic :refer [fresh
                                        ==
                                        conde
                                        !=
                                        pred
                                        run*
                                        tabled
                                        lvar?
                                        to-stream]]))

(declare db infer)

(defn synthesize-forwards [o subject predicate object level]
  (fresh [s' p' o']
    (== s' o)
    (db s' p' o')
    (conde
     [(== p' "is")]
     [(== p' "are")])
    (conde
     [(== object o')]
     [(infer s' p' o' subject predicate object (inc level))])))

(defn synthesize-backwards [o subject predicate object level]
  (fresh [s' p' o']
    (== o' o)
    (!= s' subject)
    (db s' p' o')
    (conde
     [(== p' "is")]
     [(== p' "are")])
    (conde
     [(== object s')]
     ;; new "subject", so re-query the db and recurse
     [(fresh [s'' p'' o'']
        (== s' s'')
        (db s'' p'' o'')
        (conde
         [(== p'' "is")]
         [(== p'' "are")])
        (conde
         [(== object o'')]
         [(infer s'' p'' o'' subject predicate object (inc level))]))])))

;; with a better understanding of the edges of the graph (called the
;; predicate here) other types of synthesis should be possible
;; if a > b and b > c then a > c sorts of things
;; (fresh [a b c]
;;   (db a ">" b)
;;   (db b ">" c)
;;   (== [subject predicate object] [a ">" c]))
(defn infer [s p o subject predicate object level]
  (fresh []
    (pred level (partial > 3))
    (conde
     ;; walk down the chain
     [(synthesize-forwards o subject predicate object level)]
     ;; walk up the chain
     [(synthesize-backwards o subject predicate object level)]
     [(fresh [a b c]
        (== a s)
        (conde
         [(db a ">" b)]
         [(db b "<" a)])
        (conde
         [(db b ">" c)]
         [(db c "<" b)])
        (== [subject predicate object] [a ">" c]))])))

(defn respondo [input]
  (distinct
   (run* [q]
         (fresh [subject predicate object inferred p o]
           (== q {:subject subject
                  :predicate predicate
                  :object object
                  :infered? inferred})
           (!= subject object)
           (== subject input)
           (== predicate p)
           (db input p o)
           (conde
            [(== [subject predicate object inferred]
                 [input p o false])]
            [(== inferred true)
             (infer input p o subject predicate object 0)])))))

(defn db* [subject predicate object]
  (letfn [(f [a x]
            (let [s (lp/walk a x)]
              (if (lvar? s)
                (keyword (name (gensym 'x)))
                s)))]
    (fn [a]
      (let [s (f a subject)
            p (f a predicate)
            o (f a object)]
        (assert (or (not (keyword? s)) (not (keyword? o))))
        (to-stream
         (for [rec (trip/query s p o)]
           (trampoline
            (fresh []
              (== (:subject rec) subject)
              (== (:predicate rec) predicate)
              (== (:object rec) object))
            a)))))))

(def db
  (tabled [subject predicate object]
          (db* subject predicate object)))
