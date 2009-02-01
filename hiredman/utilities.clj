(ns hiredman.utilities)

(defn get-url [x]
      (with-open [a (java.io.BufferedReader. (java.io.InputStreamReader. (.getContent (java.net.URL. x))))]
                 (.readLine a)))


