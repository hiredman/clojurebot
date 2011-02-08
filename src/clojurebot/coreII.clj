(ns clojurebot.coreII
  (:use [hiredman.clojurebot.sb :only [eval-message]]
        [conduit.core]
        [clojurebot.conduit :only [a-indirect a-if a-cond null a-when]]
        [clojure.contrib.logging :only [info]]
        [conduit.irc :only [a-irc]])
  (:require [hiredman.schedule :as sched]))

(defn addressed?
  [{:keys [bot config message type] :as bag}]
  (and (or (= type :message)
           (= type :private-message))
       (or (re-find #"^~" message)
           (re-find (re-pattern (str "^" (.getNick bot) ":")) message)
           (re-find (re-pattern (str "^" (.getNick bot) ",")) message)
           (nil? (:channel bag)))))

(def-arr remove-nick-prefix [{:keys [bot] :as bag}]
  (update-in bag [:message]
             (fn [message]
               (let [prefixes [(str (.getNick bot) ":")
                               (str (.getNick bot) ",")
                               "~"]]
                 (.trim (reduce
                         #(.replaceAll %1 (str (re-pattern %2)) "")
                         message
                         prefixes))))))

(defn question? [{:keys [message]}]
  (and message
       (> (count (.trim message)) 1)
       (= 1 (count (.split message " ")))
       (.endsWith message "?")))

(def-arr limit-length [x]
  (if (string? x)
    (let [out (apply str (take 400 x))]
      (if (> (count x) 400)
        (str out "...")
        out))
    x))

(def clojurebot-eval
  (a-comp (a-arr eval-message)
          (a-if vector?
                (a-arr
                 (fn [x]
                   (reduce #(str %1 %2 "\n") x)))
                pass-through)))

(def-arr reconnect [{:keys [server bot config]}]
  (letfn [(reconnect-fn []
            (try
              (when-not (.isConnected bot)
                (info "reconnecting")
                (.connect bot server))
              (catch Exception e
                (info "Failed to reconnect" e)
                (info "retrying in 60 seconds")
                (Thread/sleep (* 60 1000))
                reconnect-fn)))]
    (trampoline reconnect-fn)))

(def-arr rejoin [{:keys [message bot config]}]
  (doseq [c (:channels config)]
    (.joinChannel bot c)))

(def-arr nickserv-id [{:keys [bot config]}]
  (when (:nickserv-password config)
    (.sendMessage bot "nickserv" (str "identify " (:nickserv-password config)))))

(defn doc-lookup? [{:keys [message]}]
  (and message
       (.startsWith message "(doc ")))

(def math? (comp #(re-find #"^\([\+ / \- \*] [ 0-9]+\)" %)
                 str
                 :message))

(def-arr da-math [{:keys [message]}]
  (let [[op & num-strings] (re-seq #"[\+\/\*\-0-9]+" message)
        nums (map #(Integer/parseInt %) num-strings)]
    (let [out (-> (symbol "clojure.core" op)
                  (find-var)
                  (apply nums))]
      (if (> out 4)
        "*suffusion of yellow*"
        out))))

(def notice (a-arr (partial vector :notice)))

(defmulti target first)

(defmethod target :irc [[_ nick server target]]
  (a-comp (a-arr (fn [x] (info (str x)) x))
          (a-if nil?
                null
                (a-comp (a-all (a-arr (constantly target))
                               (a-arr (partial vector :notice)))
                        (a-irc server nick)))))

(defn setup-crons [config]
  (doseq [{:keys [task rate targets arguments]} (:cron config)
          :let [out (apply a-all (map target targets))]]
    (require (symbol (namespace task)))
    (info (format "scheduling cron %s %s %s %s" task rate targets arguments))
    (sched/fixedrate
     {:task #(try
               (info (format "ran cron %s" task))
               (conduit-map out [(apply @(resolve task) arguments)])
               (catch Exception e
                 (info (format "cron exception %s" task) e)))
      :start-delay (rand-int 300)
      :rate rate
      :unit (:seconds sched/unit)})))
