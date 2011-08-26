(ns clojurebot.github
  (:use [clojurebot.feed :only [atom-pull*]]))

(def url "https://github.com/%s/commits/master.atom")

(defn commits [project]
  (->> (format url project)
       atom-pull*
       (take 5)
       (map (fn [{:keys [link title author date]}]
              (format "[%s] %s - %s (%s) %s" project title author date link)))
       (reduce #(str % %2 "\n") nil)))
