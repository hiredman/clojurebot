(ns clojurebot.coreII
  (:use [clojurebot.eval :only [eval-message]]
        [conduit.core]
        [clojurebot.conduit :only [a-indirect a-if a-cond null a-when]]
        [clojure.tools.logging :only [info]]
        [conduit.irc :only [send-notice]])
  (:require [hiredman.schedule :as sched]))

(defn addressed?
  [{:keys [bot config message type] :as bag}]
  (and (or (= type :message)
           (= type :private-message))
       (or (re-find #"^~" message)
           (re-find (re-pattern (str "^" (:nick config) ":")) message)
           (re-find (re-pattern (str "^" (:nick config) ",")) message)
           (nil? (:channel bag)))))

(defn remove-nick-prefix-fn [message nick]
  (when message
    (let [message (.trim message)]
      (.trim
       (cond
        (.startsWith message (str nick ":"))
        (.replaceFirst message (str nick ":") "")

        (.startsWith message (str nick ","))
        (.replaceFirst message (str nick ",") "")

        (.startsWith message "~")
        (.replaceFirst message "~" "")

        :else
        message)))))

(def-arr remove-nick-prefix [bag]
  (prn bag)
  (update-in bag [:message] remove-nick-prefix-fn (:nick (:config bag))))

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


(defn replace-newline [st]
  (apply str
         (for [c st]
           (if (= \newline c)
             "\\n"
             c))))

(def clojurebot-eval
  (a-comp (a-arr eval-message)
          (a-if vector?
                (a-arr
                 (fn [[stdout stderr result]]
                   (let [stdout (if (empty? stdout)
                                  ""
                                  (replace-newline stdout))
                         stderr (if (empty? stderr)
                                  ""
                                  (replace-newline stderr))
                         result (when (or (not= "nil" result)
                                          (empty? stdout))
                                  (replace-newline result))]
                     (str stdout stderr result))))
                pass-through)))

(def-arr reconnect [{:keys [server bot config]}]
  (letfn [(reconnect-fn []
            (try
              (when-not (.isConnected bot)
                (info "reconnecting")
                (.connect bot server))
              (catch Exception e
                (info e "Failed to reconnect")
                (info "retrying in 60 seconds")
                (Thread/sleep (* 60 1000))
                reconnect-fn)))]
    (trampoline reconnect-fn)))

(def-arr rejoin [{:keys [message bot config]}]
  (doseq [c (:channels config)]
    (.joinChannel bot c)))

(def-arr nickserv-id [{:keys [bot config]}]
  (when (:nickserv-password config)
    (.sendMessage bot
                  "nickserv" (str "identify " (:nickserv-password config)))))

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
  (a-arr (fn [x]
           (when x
             (doseq [line (.split x "\n")]
               (send-notice target line))))))

#_(defmethod target :xmpp [[_ jid-from jid-to]]
  (when jid-to
    (let [con *xmpp-connection*
          roster (.getRoster con)
          jid-to (first
                  (map :from
                       (map bean
                            (iterator-seq
                             (.getPresences
                              roster jid-to)))))]
      (a-arr (fn [body]
               (.sendPacket con
                            (doto (new-message)
                              (.setTo jid-to)
                              (.setFrom jid-from)
                              (.setBody (str body)))))))))

(defn setup-crons [config]
  (doseq [{:keys [task rate targets arguments]} (:cron config)
          :let [out (apply a-all (map target targets))]]
    (require (symbol (namespace task)))
    (info (format "scheduling cron %s %s %s %s" task rate targets arguments))
    (sched/fixedrate
     {:task (bound-fn* #(try
                          (info (format "ran cron %s" task))
                          (conduit-map out [(apply @(resolve task) arguments)])
                          (catch Exception e
                            (info e (format "cron exception %s" task)))))
      :start-delay (rand-int 300)
      :rate rate
      :unit (:seconds sched/unit)})))
