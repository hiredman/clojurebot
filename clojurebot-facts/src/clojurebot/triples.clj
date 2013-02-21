(ns clojurebot.triples)

(declare create-store
         store-triple
         query
         delete
         db-name
         derby)

(defn ^{::ignore true} load-impl [ns]
  (require ns)
  (doseq [[n v] (ns-publics 'clojurebot.triples)
          :when (not (::ignore (meta v)))]
    (intern 'clojurebot.triples n (deref (ns-resolve ns n)))))

(load-impl 'clojurebot.triples.derby)
