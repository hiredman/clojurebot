;;
;; Thus spake the master programmer:
;;   "Though a program be but three lines long, someday it will have to be
;;   maintained."
;;
;;
;;java -server -ms16m -mx64m -Xss128m

(ns hiredman.clojurebot.core
  (:require [hiredman.pqueue :as pq]
            [hiredman.schedule :as sched]
            [hiredman.utilities :as util]
            [hiredman.words :as w])
  (:import (org.jibble.pircbot PircBot)
           (java.util Date Timer TimerTask)
           (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)
           (java.util.logging Logger)))

(defonce start-date (Date.))

(defonce task-runner sched/task-runner)

;; dictionaries for storing relationships
;; 'are' dict is not used right now.
(defonce dict-is (ref {}))
(defonce dict-are (ref {}))

;; this struct is used to pass around messages
(defstruct junks :channel :sender :login :hostname :message)

(def logger (Logger/getLogger "clojurebot"))

(declare addressed? *bots*)

(defn log [x]
  (.info logger (pr-str x)))

(defn randth
  "random item from sequence"
  [se]
  (let [s (seq se)]
    (first (drop (rand-int (count se)) se))))

;; responses that can be randomly selected from
(defonce input-accepted ["'Sea, mhuise." "In Ordnung" "Ik begrijp" "Alles klar" "Ok." "Roger." "You don't have to tell me twice." "Ack. Ack." "c'est bon!" "A nod, you know, is as good as a wink to a blind horse."])

(defonce befuddl ["Titim gan éirí ort." "Gabh mo leithscéal?" "No entiendo"  "excusez-moi" "Excuse me?" "Huh?" "I don't understand." "Pardon?" "It's greek to me." "Cool story bro."])

(defn ok
  "random input-accepted sort of string"
  []
  (randth input-accepted))

(defn befuddled
  "random \"Huh?\" sort of string"
  []
  (randth befuddl))

(defn inits "again I blame Chouser" [[f & r :as c]]
  (when c (lazy-cat (map #(conj % f)
                         (inits r)) (inits r) [(list f)])))

(defn strip-is
  "return a string with everything up to the end of the
      first \"is\" removed"
  [string]
  (.trim (.substring string (+ 3 (.indexOf string " is ")))))

(defn term
  "returns the part of a string before the first occurence
      of \"is\""
  [string]
  (first (.split string " is ")))

(defn doc-lookup?
  "is this a well formed doc-string lookup?"
  [msg]
  (re-find #"^\(doc " msg))

(defn d?op
  "if string ends in a question mark return
      the string without the question mark"
  [x]
  (.replaceAll x "^(.*)\\?$" "$1"))

(defn- normalise-docstring
  [string]
  (and string (.replaceAll string "\\s+" " ")))

(defn symbol-to-var-doc
  "this returns the doc metadata from a var in the
      clojure ns or a befuddled response"
  [symb]
  (let [a (meta (find-var (symbol "clojure.core" symb)))
        x (normalise-docstring (:doc a))
        y (:arglists a)]
    (if x
      (str x (when y (str "; arglists " y)))
      (befuddled))))

(defmacro async
  "just do this, I don't care"
  [& x]
  `(send-off (agent nil) (fn [& _#] ~@x )))

(defn who
  "am I talking to someonein a privmsg, or in a channel?"
  [pojo]
  (or (:channel pojo) (:sender pojo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sendMsg
  "send a message to a recv, a recv is a channel name or a nick"
  [this recv msg]
  (io! (.sendMessage this recv (.replace (str msg) \newline \space))))

(defn sendMsg-who
  "wrapper around sendMsg"
  [bot msg msg-to-send]
  (sendMsg (:this bot) (who msg) msg-to-send))

(defmulti send-out (fn [& x] (first x)))

(defmethod send-out :msg [_ bot recvr string]
  (io! (.sendMessage #^PircBot (:this bot) (if (map? recvr) (who recvr) recvr) (normalise-docstring (str string)))))

(defmethod send-out :action [_ bot recvr string]
  (io! (.sendAction #^PircBot (:this bot) (if (map? recvr) (who recvr) recvr) (normalise-docstring (str string)))))

(defmethod send-out :notice [_ bot recvr string]
  (io! (.sendNotice #^PircBot (:this bot) (if (map? recvr) (who recvr) recvr) (normalise-docstring (str string)))))

(defmulti new-send-out (comp type first list))

(defn send-out [one two & r]
  (apply new-send-out two one r))

(defmethod new-send-out clojure.lang.IPersistentMap [bot msg-type recvr message]
  (condp = msg-type
    :msg
    (io! (.sendMessage #^PircBot (:this bot) (if (map? recvr) (who recvr) recvr) (normalise-docstring (.toString message))))
    :action
    (io! (.sendAction #^PircBot (:this bot) (if (map? recvr) (who recvr) recvr) (normalise-docstring (.toString message))))
    :notice
    (io! (.sendNotice #^PircBot (:this bot) (if (map? recvr) (who recvr) recvr) (normalise-docstring (.toString message))))))

(defmethod new-send-out :irc [bot msg-type recvr message]
  (new-send-out (vary-meta bot dissoc :type) msg-type recvr message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-channels [bot fn]
  (doseq [c (.getChannels (:this bot))]
    (fn c)))

;; (defn store [bot key value]
;;   (trip/delete (trip/derby (db-name bot)) key "is" :y)
;;   (trip/store-triple (trip/derby (db-name bot)) {:s key :o value :p "is"}))

;; (defn what-is [term bot]
;;   (trip/query (trip/derby (db-name bot)) term :x :y))


(defmacro dfn
  "Creates a dispatch fn with 'bot bound to the bot object
   and 'msg bound to a struct representing the message"
  [& body]
  `(fn [~'bot ~'msg]
     ~@body))

(defn everyone-I-see
  "returns seq like ([\"#clojure\" (\"somenick\" \"someothernicl\")])
      for ever channel the bot is in"
  [bot]
  (for [channel (.getChannels bot)]
    [channel (map (comp :nick bean) (.getUsers bot channel))]))

(defn see-nick?
  "do I see someone with the nickname nick? returns nil or a seq of channels where I see him"
  [bot nick]
  (reduce #(when %2 (if (seq? %) (conj % %2) (filter identity [% %2])))
          nil
          (map first (filter #(some (fn [y] (.equals y nick)) (last %))
                             (everyone-I-see bot)))))

(defn random-person [bot]
  (randth (filter #(not (.equals % (:nick bot)))
                  (apply concat (map last (everyone-I-see bot))))))

(def #^{:doc "ref contains priority queue that is used for dispatching the responder multimethod"}
  *dispatchers*
  (ref pq/empty))

(defn dispatch
  "this function does dispatch for responder"
  [bot msg]
  (loop [d (pq/seq @*dispatchers*)]
    (when d
      (let [[k v] (first d)]
        (if (k bot msg)
          v
          (recur (seq (rest d))))))))

(defn add-dispatch-hook
  "Allows you to add your own hook to the message responder
   You *must* define a 'responder multimethod corresponding to the
   dispatch-value"
  ([dispatch-check dispatch-value]
     (add-dispatch-hook 0 dispatch-check dispatch-value))
  ([dispatch-priority dispatch-check dispatch-value]
     (dosync (commute *dispatchers* pq/conj dispatch-priority [dispatch-check dispatch-value]))))

(defn remove-dispatch-hook [dispatch-value]
  (dosync
   (alter
    *dispatchers*
    (comp (partial into pq/empty)
          (partial filter #(not= dispatch-value (last (last %))))))))

;; register legacy stuffs
(dorun
 (map #(add-dispatch-hook 0 (first %) (second %))
      [[(dfn (doc-lookup? (:message msg))) ::doc-lookup]
       [(dfn (re-find #"^\([\+ / \- \*] [ 0-9]+\)" (:message msg))) ::math]]))

;;this stuff needs to come last?
                                        ;(add-dispatch-hook 20 (dfn (and (addressed? bot msg) (not (:quit msg)))) ::lookup)

(defmacro defresponder [key priority fn & body]
  `(do
     (defmethod responder ~key [~'bot ~'msg]
       (let [~'msg (vary-meta ~'msg assoc ~key true)]
         ~@body))
     (add-dispatch-hook ~priority (dfn (when (not (~key (meta ~'msg))) (~fn ~'bot ~'msg))) ~key)))

;;(defresponder2
;;  {:priority 1
;;   :key ::dostuff
;;   :dispatch (fn [bot msg])
;;   :body (fn [bot msg])})

(defmacro defresponder2 [{:keys [priority body dispatch name]}]
  `(let [priority# ~priority
         body# ~body
         dispatch# ~dispatch
         name# ~name]
     (remove-dispatch-hook name#)
     (defmethod responder name# [bot# msg#] (body# bot# msg#))
     (add-dispatch-hook priority#
                        (fn [bot# msg#]
                          (when (not (name# (meta msg#)))
                            (dispatch# bot# msg#)))
                        name#)))

(defmulti #^{:doc "currently all messages are routed though this function"} responder dispatch)

(defmethod responder nil [& _])

(defn remove-from-beginning
  "return a string with the concatenation of the given chunks removed if it is
   found at the start of the string"
  [string & chunks]
  (.replaceFirst string (apply str "^" chunks) ""))

(defn extract-message
  "removes bot name and/or ~ from the beginning of the msg"
  [bot pojo]
  (.trim (.replaceAll (:message pojo) (str "(?:" (:nick bot) ":|~)(.*)") "$1")))

;; (defmethod responder ::know [bot pojo]
;;   (new-send-out bot :msg pojo (str "I know "  (count (trip/query (trip/derby (db-name bot)) :y :y :z))" things")))

(defn handleMessage [this channel sender login hostname message]
  (try
    (let [bot this
          msg (struct junks channel sender login hostname message)]
      (future (trampoline responder bot (vary-meta msg assoc :addressed? (addressed? bot msg)))))
    (catch Exception e (.printStackTrace e))))

(defn handlePrivateMessage [this sender login hostname message]
  (handleMessage this nil sender login hostname message))

(defn join-or-part [this event channel sender login hostname]
  (try
    (trampoline responder this
                (assoc (struct junks channel sender login hostname "") event true))
    (catch Exception e (.printStackTrace e))))

(defn pircbot [bot-config]
  (let [x (promise)
        bot-obj
        (proxy [PircBot] []
          (onJoin [channel sender login hostname]
            (join-or-part @x :join channel sender login hostname))
          (onPart [channel sender login hostname]
            (join-or-part @x :part channel sender login hostname))
          (onQuit [nick login hostname reason]
            (join-or-part @x :quit nil nick login hostname))
          (onMessage [channel sender login hostname message]
            (handleMessage @x channel sender login hostname message))
          (onPrivateMessage [sender login hostname message]
            (handlePrivateMessage @x sender login hostname message)))]
    (let [w (merge bot-config {:this bot-obj})]
      (x w)
      w)))

(defn dict-file [config suffix]
  (let [file (-> (str (:dict-dir config "./") (:dict-basename config (:nick config)) suffix)
                 java.io.File.)]
    (.createNewFile file)
    file))

(defn dump-dicts [config]
  (dorun (map (fn [[rel rels]]
                (binding [*out* (java.io.FileWriter.
                                 (dict-file config rel))]
                  (prn @rels)
                  (.close *out*)))
              [[".is" dict-is] [".are" dict-are]])))

(defn load-dicts [config]
  (dosync
   (ref-set dict-is
            (eval
             (binding [*in* (java.io.PushbackReader.
                             (java.io.FileReader.
                              (dict-file config ".is")))]
               (let [a (try (read) (catch Exception e {}))]
                 (.close *in*)
                 a)))))
  config)

(defn dump-dict-is [config]
  (log "Dumping dictionaries")
  (binding [*out* (-> (dict-file config ".is")
                      java.io.FileWriter.)]
    (prn @dict-is)
    (.close *out*)))

(defn load-store [bot]
  (send (:store bot)
        (fn [& _]
          (log "Reading store")
          (binding [*in* (-> (dict-file bot ".store") java.io.FileReader. java.io.PushbackReader.)]
            (with-open [i *in*]
              (try (read)
                   (catch Exception e
                     (println e)))))))
  bot)

(defn watch-store [bot]
  (add-watch (:store bot)
             :writer
             (fn [key ref old-state new-state]
               (log "Writing store")
               (binding [*out* (-> (dict-file bot ".store") java.io.FileWriter.)]
                 (with-open [o *out*] (prn new-state)))))
  bot)

(defn start-dump-thread [config]
  (sched/fixedrate
   {:task #(dump-dict-is config)
    :start-delay 1
    :rate 10
    :unit (:minutes sched/unit)})
  config)

(defn wall-hack-method [class-name name- params obj & args]
  (-> class-name (.getDeclaredMethod (name name-) (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(defn start-clojurebot [attrs additional-setup]
  (let [bot (pircbot attrs)]
    (dosync (commute *bots* assoc (:this bot) bot))
    (wall-hack-method org.jibble.pircbot.PircBot :setName [String] (:this bot) (:nick bot))
    (doto (:this bot)
      (.connect (:network bot))
      (.changeNick (:nick bot))
      (.joinChannel (:channel bot)))
    (additional-setup bot)
    bot))

(defmacro run-clojurebot [botname botattrs & additional-setup]
  `(start-clojurebot ~botattrs (fn [~botname] (do ~@additional-setup))))
