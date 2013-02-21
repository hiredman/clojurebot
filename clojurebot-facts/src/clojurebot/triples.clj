(ns clojurebot.triples)

(declare create-store
         store-triple
         query
         delete)

(defn ^{::ignore true} load-impl [ns]
  (require ns)
  (doseq [[n v] (ns-publics 'clojurebot.triples)
          :when (not (::ignore (meta v)))]
    (intern 'clojurebot.triples n (deref (ns-resolve ns n)))))

(load-impl (symbol (or (System/getProperty "factoid.storage")
                       "clojurebot.triples.derby")))
