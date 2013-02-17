(ns clojurebot.triples
  (:require [clojure.java.jdbc :as sql]
            [clojure.tools.logging]))

(defmacro with-c [db & body]
  `(if (:connection clojure.java.jdbc.internal/*db*)
     (do
       ~@body)
     (sql/with-connection ~db
       ~@body)))

(defn derby [name]
  {:classname "org.apache.derby.jdbc.EmbeddedDriver"
   :create true
   :subname name
   :subprotocol "derby"})

(defn create-store
  [name]
  (with-c (derby name)
    (sql/create-table
     :triples
     [:id :int "PRIMARY KEY" "GENERATED ALWAYS AS IDENTITY"]
     [:subject "varchar(32670)"]
     [:predicate "varchar(32670)"]
     [:object "varchar(32670)"]
     [:upper_subject "varchar(32670)"]
     [:created_at :timestamp "NOT NULL" "DEFAULT CURRENT_TIMESTAMP"])))

;; TODO: remove this hack once this is fixed upstream
(in-ns 'clojure.java.jdbc.internal)

(defn do-prepared-return-keys*
  "Executes an (optionally parameterized) SQL prepared statement on the
  open database connection. Each param-group is a seq of values for all of
  the parameters.
  Return the generated keys for the (single) update/insert."
  [^String sql & param-groups]
  (with-open [^PreparedStatement stmt (prepare-statement* (connection*) sql :return-keys true)]
    (doseq [param-group param-groups]
      (set-parameters stmt param-group)
      (.addBatch stmt))
    (transaction* (fn []
                    (let [counts (.executeBatch stmt)]
                      (try
                        (first (resultset-seq* (.getGeneratedKeys stmt)))
                        (catch Exception _
                          ;; assume generated keys is unsupported and return counts instead:
                          counts)))))))

(in-ns 'clojurebot.triples)

(defn store-triple [db {:keys [s p o]}]
  (with-c db
    (sql/transaction
     (sql/insert-values
      :triples
      [:subject :predicate :object :upper_subject]
      [(.trim (str s)) (.trim (str p)) (.trim (str o))
       (.toUpperCase (.trim (str s)))]))))

(defmulti query
  (fn [db s p o]
    (cond
     (and (list? s) (keyword? p) (keyword? o))
     ::like_subject-_-_
     (and (not (keyword? s)) (not (keyword? p)) (keyword? o))
     ::subject-predicate-_
     (and (keyword? s) (keyword? p) (not (keyword? o)))
     ::_-_-object
     (and (keyword? s) (not (keyword? p)) (keyword? o))
     ::_-predicate-_
     (and (not (keyword? s)) (keyword? p) (keyword? o))
     ::subject-_-_
     (and (keyword? s) (keyword? p) (keyword? o))
     ::_-_-_
     (and (keyword? s) (not (keyword? p)) (not (keyword? o)))
     ::_-predicate-object
     :else
     ::subject-predicate-object)))

(defmethod query ::subject-_-_ [db s p o]
  (prn db)
  (try
    (with-c db
      (sql/with-query-results res
        ["SELECT * FROM triples WHERE upper_subject = ?" (.toUpperCase s)]
        (doall res)))
    (catch Exception e
      (println db s p o)
      (throw e))))

(defmethod query ::like_subject-_-_ [db s p o]
  (try
    (with-c db
      (sql/with-query-results res
        ["SELECT * FROM triples WHERE upper_subject LIKE ?"
         (.toUpperCase (first s))]
        (doall res)))
    (catch Exception e
      (println db s p o)
      (throw e))))

(defmethod query ::_-predicate-_ [db s p o]
  (with-c db
    (sql/with-query-results res
      ["SELECT * FROM triples WHERE predicate = ?" p]
      (doall res))))

(defmethod query ::subject-predicate-object [db s p o]
  (try
    (with-c db
      (sql/with-query-results res
        [(str "SELECT * FROM triples WHERE "
              "predicate = ? AND "
              "upper_subject = ? AND "
              "object = ?")
         p (.toUpperCase s) o]
        (doall res)))
    (catch Exception e
      (println db s p o)
      (throw e))))

(defmethod query ::subject-predicate-_ [db s p o]
  (clojure.tools.logging/info "QUERY" s p o)
  (try
    (with-c db
      (sql/with-query-results res
        [(str "SELECT * FROM triples WHERE "
              "predicate = ? AND "
              "upper_subject = ?")
         p (.toUpperCase s)]
        (doall res)))
  (catch Exception e
    (println db s p o)
    (throw e))))

(defmethod query ::_-predicate-object [db s p o]
  (clojure.tools.logging/info "QUERY" s p o)
  (with-c db
    (sql/with-query-results res
      [(str "SELECT * FROM triples WHERE "
            "predicate = ? AND "
            "object = ?")
       p o]
      (doall res))))

(defmethod query ::_-_-_ [db s p o]
  (with-c db
    (sql/with-query-results res
      ["SELECT * FROM triples"]
      (doall res))))

(defn delete [db s p o]
  (doseq [id (map :id (query db s p o))]
    (with-c db
      (sql/delete-rows :triples ["id = ?" id]))))

(defn string [{:keys [subject predicate object]}]
  (if (.startsWith object "<reply>")
    (.trim (subs object 7))
    (format "%s %s %s" subject predicate object)))

(defn import-file [db file]
  (binding [*in* (-> file java.io.File. java.io.FileReader.
                     java.io.PushbackReader.)]
    (-> (map (fn [[s o]] {:s s :o o :p "is"}) (read))
        ((partial mapcat
                  (fn [x]
                    (if (-> x :o vector?)
                      (map #(assoc x :o %) (:o x))
                      [x]))))
        ((partial map #(doto % prn)))
        ((partial map (partial store-triple db)))
        doall)))

(defn db-name []
  (let [name (or (System/getProperty "clojurebot.db")
                 (str (System/getProperty "user.dir")
                      "/bot.db"))]
    (when-not (.exists (java.io.File. name))
      (create-store name))
    name))
