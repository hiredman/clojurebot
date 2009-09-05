(ns hiredman.words)

(defn drop-nth [s n]
  (lazy-seq
    (if (zero? n)
      (rest s)
      (cons (first s) (drop-nth (rest s) (dec n))))))

(defn permute [items]
  (cond
    (> 2 (count items))
      list
    (= 2 (count items))
      (let [[a b] items] (list (list a b) (list b a)))
    :else
      (mapcat (fn [idx]
             (map (partial cons (nth items idx)) (permute (drop-nth items idx))))
           (range (count items)))))

(defn word-seq [sentence]
  (seq (set (re-seq #"\w+" sentence))))

(defn chunk-words [words]
  (take-while (comp not empty?) (iterate rest words)))

(defn word-seq->sentence [ws]
  (apply str (interpose " " ws)))

(defn sentence-permutations [sentence]
    (-> sentence word-seq permute ((partial mapcat chunk-words))
      set
      ((partial map word-seq->sentence))
      ((partial sort-by count)) reverse))
