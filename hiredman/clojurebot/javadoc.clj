(ns hiredman.clojurebot.javadoc
    (:use (hiredman.clojurebot core))
    (:use (hiredman utilities)))

(def doc-url "http://www.docjar.com/docs/api/")

(defn make-url [class]
      (str doc-url (.replaceAll (if (.contains class ".") class (str "java.lang." class) ) "\\." "/") ".html"))

(defmethod responder ::javadoc [bot msg]
  (let [message (extract-message bot msg)
        thing (second (.split #^String message " "))]
    (send-out :notice bot (who msg) (str (make-url thing)))))

(add-dispatch-hook (dfn (and (addressed? bot msg)
                             (re-find #"^(jdoc|javadoc) " (extract-message bot msg)))) ::javadoc)
