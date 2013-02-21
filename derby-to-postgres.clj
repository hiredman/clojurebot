(let [pom-uber-jar
      (str "http://thelibraryofcongress.s3.amazonaws.com/"
           "pomegranate-0.0.13-SNAPSHOT-jar-with-dependencies.jar")
      cl (java.net.URLClassLoader. (into-array [(java.net.URL. pom-uber-jar)]))
      cx (.getContextClassLoader (Thread/currentThread))]
  (push-thread-bindings {clojure.lang.Compiler/LOADER cl})
  (.setContextClassLoader (Thread/currentThread) cl)
  (try
    (require '[cemerick.pomegranate :as pom])
    (finally
      (.setContextClassLoader (Thread/currentThread) cx)
      (pop-thread-bindings))))

(pom/add-dependencies :coordinates '[[postgresql/postgresql "9.1-901.jdbc4"]
                                     [org.apache.derby/derby "10.8.1.2"]
                                     [org.clojure/java.jdbc "0.2.3"]]
                      :repositories (merge cemerick.pomegranate.aether/maven-central
                                           {"clojars" "http://clojars.org/repo"}))

(def derby-db
  {:classname "org.apache.derby.jdbc.EmbeddedDriver"
   :subname (first *command-line-args*)
   :subprotocol "derby"})

(def postgres-db
  (second *command-line-args*))

(def postgres-table (last *command-line-args*))

(require '[clojure.java.jdbc :as jdbc])

(jdbc/with-connection postgres-db
  (try
    (jdbc/do-commands "CREATE EXTENSION \"uuid-ossp\"")
    (catch Exception _))
  (jdbc/create-table
   postgres-table
   [:id :uuid "PRIMARY KEY" "DEFAULT uuid_generate_v4()"]
   [:subject "varchar(32670)"]
   [:predicate "varchar(32670)"]
   [:object "varchar(32670)"]
   [:upper_subject "varchar(32670)"]
   [:created_at :timestamp "NOT NULL" "DEFAULT CURRENT_TIMESTAMP"])
  (apply jdbc/insert-records
         postgres-table
         (map #(dissoc % :id)
              (jdbc/with-connection derby-db
                (jdbc/with-query-results results
                  ["SELECT * FROM triples"]
                  (doall results))))))
