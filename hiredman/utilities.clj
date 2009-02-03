(ns hiredman.utilities)

(defn get-url [x]
      (with-open [a (java.io.BufferedReader. (java.io.InputStreamReader. (.getContent (java.net.URL. x))))]
                 (.readLine a)))

(defmacro mk-interface [class fn]
  (let [x (map #(list (symbol (.getName %))
                      ['& 'x]
                      (list 'let ['method (keyword (.getName %))] fn)) (.getMethods (eval class)))]
    `(proxy [~class] [] ~@x)))
