;; DEPENDS clojureql apache-derby
(ns hiredman.triples
  (:require [dk.bestinclass.clojureql :as cql]  
            [dk.bestinclass.clojureql.backend.derby :as cql-derby]
            [clojure.contrib.sql :as sql]))

(defn derby [name]
  (assoc (cql/make-connection-info "derby" name nil nil)
         :classname "org.apache.derby.jdbc.EmbeddedDriver"
         :create true
         :subname name
         :subprotocol "derby"))

(defn create-store
    [name]
  (sql/with-connection (derby name)
    (sql/create-table
          :triples
          [:id :int "PRIMARY KEY" "GENERATED ALWAYS AS IDENTITY"]
          [:subject "varchar(32670)"]
          [:predicate "varchar(32670)"]
          [:object "varchar(32670)"]
          [:upper_subject "varchar(32670)"]
          [:created_at :timestamp "NOT NULL" "DEFAULT CURRENT_TIMESTAMP"])))

(defn store-triple [db {:keys [s p o]}]
  (cql/run [db results]
           (cql/insert-into triples
                            [:subject ~(.trim (.toString s)) :predicate ~(.trim (.toString p)) :object ~(.trim (.toString o))
                             :upper_subject ~(.toUpperCase (.trim (.toString s)))])
           results))

(defmulti query
          (fn [db s p o]
            (cond
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
              :else
                ::subject-predicate-object)))

(defmethod query ::subject-_-_ [db s p o]
  (cql/run [db results]
           (cql/query triples * (= ~(.toUpperCase s) upper_subject))
           (doall results)))

(defmethod query ::_-predicate-_ [db s p o]
  (cql/run [db results]
           (cql/query triples * (= ~p predicate))
           (doall results)))

(defmethod query ::subject-predicate-object [db s p o]
  (cql/run [db results]
           (cql/query triples * (and (= ~(.toUpperCase s) upper_subject)
                                     (= ~p predicate)
                                     (= ~o object)))
           (doall results)))

(defmethod query ::subject-predicate-_ [db s p o]
  (cql/run [db results]
           (cql/query triples * (and (= ~(.toUpperCase s) upper_subject)
                                     (= ~p predicate)))
           (doall results)))

(defmethod query ::_-_-_ [db s p o]
  (cql/run [db results]
           (cql/query triples *)
           (doall results)))

(defn delete [db s p o]
  (doseq [id (map :id (query db s p o))]
    (cql/run [db results]
             (cql/delete-from triples (= id ~id))
             results)))

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
