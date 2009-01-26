(ns hiredman.horizon)

(declare *fails* *succeeds* *exits*)
(def *horizon* nil)

(defmacro when-hrz [which & forms]
  `(do
     (when (not *horizon*) (throw (RuntimeException. "Naked Singularity")))
     (swap!
       ~(condp = which :fails '*fails* :succeeds '*succeeds* :exits '*exits*)
       conj (fn [] ~@forms))))

(defmacro horizon [& body]
  `(binding [*horizon* true
             *fails* (atom (list))
             *succeeds* (atom (list))
             *exits* (atom (list))]
            (try
              (let [y# (do ~@body)] (dorun (map (fn[x#](x#)) @*succeeds*)) y#)
              (catch Exception e# (dorun (map (fn[x#] (x#)) @*fails*))
                     (throw e#))
              (finally
                (dorun (map (fn[x#] (x#)) @*exits*))))))
