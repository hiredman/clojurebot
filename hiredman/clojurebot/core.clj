;;
;; Thus spake the master programmer:
;;   "Though a program be but three lines long, someday it will have to be
;;   maintained."
;; 
;;
;; [01:30] <uhelp> lexxan: Since Mon May  2 17:22:46 2005,
;;                 there have been 0 modifications and 0 questions.
;;                 I have been awake for 7 minutes and 36 seconds
;;                 this session, and currently reference 19
;;                 factoids. Addressing is in optional mode.


;java -server -ms16m -mx64m -Xss128m

(ns hiredman.clojurebot.core
    (:use (hiredman sandbox))
    (:require [hiredman.pqueue :as pq])
    (:import (org.jibble.pircbot PircBot)
             (java.util.concurrent FutureTask TimeUnit TimeoutException)))

(def *bots* (ref {})) ; This will hold bot objects
(def start-date (java.util.Date.))

;; dictionaries for storing relationships
;; 'are' dict is not used right now.
(def dict-is (ref {}))
(def dict-are (ref {}))

;url is for storing urls, must figure out something to do with this
(def url (ref {}))

(def url-regex #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

;; this struct is used to pass around messages
(defstruct junks :channel :sender :login :hostname :message)

(defn randth
      "random item from sequence"
      [se]
      (let [s (seq se)]
        (first (drop (rand-int (count se)) se))))

;; responses that can be randomly selected from
(def input-accepted ["'Sea, mhuise." "In Ordnung" "Ik begrijp" "Alles klar" "Ok." "Roger." "You don't have to tell me twice." "Ack. Ack." "c'est bon!"])
(def befuddl ["Titim gan éirí ort." "Gabh mo leithscéal?" "No entiendo"  "excusez-moi" "Excuse me?" "Huh?" "I don't understand." "Pardon?" "It's greek to me."])

(defn ok
      "random input-accepted sort of string"
      []
      (randth input-accepted))

(defn befuddled
      "random \"Huh?\" sort of string"
      []
      (randth befuddl))

(defn inits
      "this is Chouser's fault"
      [s]
      (map first
           (take-while second
                       (map split-at
                            (iterate inc 0)
                            (repeat (lazy-cat s [nil]))))))


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
      (if (= \? (.charAt x (dec (count x))))
        (subs x 0 (dec (count x)))
        x))

(defn symbol-to-var-doc
      "this returns the doc metadata from a var in the
      clojure ns or a befuddled response"
      [symb]
      (let [a (meta (find-var (symbol "clojure.core" symb)))
            x (:doc a)
            y (:arglists a)]
        (if x
          (str x "; arglists " y)
          (befuddled))))

(defmacro async
  "just do this, I don't care"
  [& x]
  `(send-off (agent nil) (fn [& _#] ~@x )))

(defn who
      "am I talking to someonein a privmsg, or in a channel?"
      [pojo]
      (if (:channel pojo)
        (:channel pojo)
        (:sender pojo)))

(defn sendMsg
      "send a message to a recv, a recv is a channel name or a nick"
      [this recv msg]
      (.sendMessage this recv (.replace (str msg) \newline \space)))

(defn sendMsg-who
      "wrapper around sendMsg"
      [bot msg msg-to-send]
  (sendMsg (:this bot) (who msg) msg-to-send))

(defn term-lists
      "generates permutions of the words in string"
      [msg words-to-ignore]
      (let [x (re-seq #"\w+" msg)
            ignore #(not ((set words-to-ignore) %))]
        (filter ignore
        (apply concat
               (map (fn [x]
                        (map (fn [y]
                                 (reduce #(str % " " %2) y)) x))
                    (map #(reverse (filter identity (inits (drop % x))))
                         (take (count x) (iterate inc 0))))))))

(defn rlookup
      "look up terms from a seq until you find a defi"
      [terms]
      (loop [t terms]
            (if t
              (if (@dict-is (first t))
                (first t)
                (recur (rest t))))))

(defn fuzzy-lookup
      "look up based on permutation"
      [message words-to-ignore]
      (rlookup (term-lists message words-to-ignore)))

(defn fuzzy-key-lookup
      "look up based on match part of a term"
      [term]
      (randth (filter #(when (> (.lastIndexOf % term) -1) true) (keys @dict-is))))


(defn addressed?
      "is this message prefixed with clojurebot: "
      [bot msg]
      (when (or (re-find (re-pattern (str "^" (:nick bot) ":")) (:message msg)) (nil? (:channel msg)))
        msg))



(defn is
      "add a new definition to a term in dict-is"
      [term defi]
      (if (@dict-is term)
        (let [old (@dict-is term)
              v (if (vector? old)
                  (conj old defi)
                  [old defi])]
          (dosync (commute dict-is assoc term v)))
        (dosync (commute dict-is assoc term defi))))

(defn is!
      "define a term in dict-is, overwriting anything that was there"
      [term defi]
      (dosync (commute dict-is assoc term defi)))


(defn what-is
      "looks up a term in @dict-is"
      [term]
      (when-let [f (@dict-is term)]
        (if (vector? f) (randth f) f)))

(defmacro dfn 
  "Creates a dispatch fn with 'bot bound to the bot object
   and 'msg bound to a struct representing the message"
  [& body]
  `(fn [~'bot ~'msg]
     ~@body))

;; (def *dispatchers* 
;;      (ref 
;;        [(dfn (doc-lookup? (:message msg))) 
;;         ::doc-lookup,
;;         (dfn (re-find #"^,\(" (:message msg))) 
;;         ::code-sandbox,
;;         (dfn (and (addressed? bot msg) 
;;               (re-find #"how much do you know?" (:message msg))))
;;         ::know
;;         (dfn (and (addressed? bot msg) (re-find #" is " (:message msg))  
;;                   (not= \? (last (:message msg)))))
;;         ::define-is
;;         (dfn (re-find #"^\([\+ / \- \*] [ 0-9]+\)" (:message msg)))
;;         ::math
;;         (dfn (addressed? bot msg))
;;         ::lookup
;;         (dfn (re-find url-regex (:message msg)))
;;         ::url]))

(def *dispatchers*
     (ref '()))


(defn dispatch
      "this function does dispatch for responder"
      [bot msg]
      (loop [d @*dispatchers*]
        (when d
          (let [k (first (pq/first d))
                v (second (pq/first d))]
            (if (k bot msg)
              v
              (recur (rest d)))))))


(defn add-dispatch-hook
  "Allows you to add your own hook to the message responder
   You *must* define a 'responder multimethod corresponding to the
   dispatch-value"
  ([dispatch-priority dispatch-check dispatch-value]
   (dosync (commute *dispatchers* pq/conj [dispatch-priority [dispatch-check dispatch-value]])))
  ([dispatch-check dispatch-value]
   (add-dispatch-hook 0 dispatch-check dispatch-value)))
 
;; register legacy stuffs
(dorun
  (map #(add-dispatch-hook (first %) (second %))
       [[(dfn (doc-lookup? (:message msg))) ::doc-lookup]
        [(dfn (and (addressed? bot msg) 
              (re-find #"how much do you know?" (:message msg)))) ::know]
        [(dfn (and (addressed? bot msg) (re-find #" is " (:message msg))  
                  (not= \? (last (:message msg))))) ::define-is]
        [(dfn (re-find #"^\([\+ / \- \*] [ 0-9]+\)" (:message msg))) ::math]]))

;;this stuff needs to come last?
(add-dispatch-hook 20 (dfn (addressed? bot msg)) ::lookup)
(add-dispatch-hook 21 (dfn (re-find url-regex (:message msg))) ::url)

(defmulti #^{:doc "currently all messages are routed though this function"} responder dispatch)

(defn naughty-forms? [strang]
      (let [nf #{"catch" "finally" "clojure.asm" "hiredman.clojurebot"}]
        (some #(not= -1 %) (map #(.lastIndexOf strang %) nf))))

;; unused, unneeded
;; (defn find-or-create-ns [n]
;;       (if-let [s (find-ns n)] s (create-ns n)))

(defmethod responder ::code-sandbox [bot pojo]
  (println (str (:sender pojo) " " (:message pojo)))
  (if (and (not (naughty-forms? (:message pojo))) (not= "karmazilla" (:sender pojo)))
    (let [result (try (eval-in-box (.replaceAll (:message pojo) "^," "")
                              (:sandbox-ns bot))
                      (catch Exception e
                             (str "Eval-in-box threw an exception:" (.getMessage e))))
          _ (println "Result:" result)]
      (if (vector? result)
        (doseq [i (reverse result)]
           (sendMsg-who bot pojo i))
        (sendMsg-who bot pojo (.replace result "(NO_SOURCE_FILE:0)" ""))))
  (sendMsg-who bot pojo (befuddled))))

(defmethod responder ::math [bot pojo]
  (let [[op & num-strings] (re-seq #"[\+\/\*\-0-9]+" (:message pojo))
        nums (map #(Integer/parseInt %) num-strings)]
    (sendMsg-who bot pojo
                 (let [out (apply  (find-var (symbol "clojure.core" op)) nums)]
                   (if (> out 4)
                     "*suffusion of yellow*"
                     out)))))

(defmethod responder ::doc-lookup [bot pojo]
  (sendMsg-who bot pojo
               (symbol-to-var-doc (subs (:message pojo)
                                        5
                                        (dec (count (:message pojo)))))))
(defn remove-from-beginning
  "return a string with the concatenation of the given chunks removed if it is
   found at the start of the string"
  [string & chunks]
  (.replaceFirst string (apply str "^" chunks) ""))

(defmethod responder ::define-is [bot pojo]
  (let [a (.trim (remove-from-beginning (:message pojo) (:nick bot) ":"))
        term (term a)
        x (strip-is a)
        defi (remove-from-beginning x "also ")]
    (if (re-find #"^also " x)
      (is term defi)
      (is! term defi))
    (sendMsg-who bot pojo (ok))))

(defn prep-reply
      "preps a reply, does substituion of stuff like <reply> and #who"
      [sender term defi]
      (.replaceAll (if (re-find #"^<reply>" defi)
                     (.trim (remove-from-beginning (str defi) "<reply>"))
                     (str term " is " defi))
                   "#who"
                   sender))

(defmethod responder ::lookup [bot pojo]
  (let [msg (d?op (.trim (remove-from-beginning (:message pojo) (:nick bot) ":")))
        result (what-is msg)
        words-to-ignore ["a" "where" "what" "is" "who" "are" (:nick bot)]]
    (cond
      result,
        (sendMsg-who bot pojo
                     (.replaceAll (if (re-find #"^<reply>" result)
                                    (.trim (remove-from-beginning (str result) "<reply>"))
                                    (str msg " is " result))
                                  "#who"
                                  (:sender pojo)))

      (fuzzy-lookup msg words-to-ignore),
        (let [term (fuzzy-lookup msg words-to-ignore)
              defi (what-is term)]
          (sendMsg-who bot pojo (prep-reply (:sender pojo) term defi)))

      (fuzzy-key-lookup msg),
        (let [term (fuzzy-key-lookup msg)
              defi (what-is term)]
          (sendMsg-who bot pojo (prep-reply (:sender pojo) term defi)))

      :else,
        (sendMsg-who bot pojo (befuddled)))))


(defmethod responder ::know [bot pojo]
  (sendMsg-who bot pojo (str "I know " (+ (count (deref dict-is)) (count (deref dict-are))) " things")))

(defmethod responder ::url [bot pojo]
  (dosync (commute url
                   assoc
                   (re-find url-regex (:message pojo)) (java.util.Date.)))
  (prn (str (:sender pojo) ", " (:message pojo))))

(defmethod responder ::literal [bot pojo]
  (let [q (remove-from-beginning (:message pojo) (:nick bot) ": literal ")]
    (prn q)))


(defn user-watch [this]
      (let [cur (count (.getUsers this "#clojure"))
            pre (Integer/parseInt (what-is "max people"))]
        (println cur)
        (println pre)
        (when (> cur pre)
          (is! "max people" (str cur)))))


(defn handleMessage [this channel sender login hostname message]
  (let [bot (get @*bots* this)]
  (responder bot (struct junks channel sender login
                         hostname message))))

(defn handlePrivateMessage [this sender login hostname message]
      (handleMessage this nil sender login hostname message))

(defn pircbot [bot-config]
  (let [bot-obj 
        (proxy [PircBot] []
          (onJoin [channel sender login hostname]
                  (user-watch this))
          (onMessage [channel sender login hostname message]
                     (handleMessage this channel sender login hostname message))
          (onPrivateMessage [sender login hostname message]
                            (handlePrivateMessage this sender login hostname message)))]
    (merge bot-config {:this bot-obj})))


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

;(svn-message (svn-summaries (clojure.xml/parse (svn-xml-stream))))
    
(defn load-dicts [config]
  (dosync
   (ref-set dict-is
            (eval
             (binding [*in* (java.io.PushbackReader. 
                             (java.io.FileReader.
                              (dict-file config ".is")))]  
               (let [a (try (read) (catch Exception e {}))]
                 (.close *in*)
                 a))))))

(defn start-dump-thread [config]
  (send-off (agent nil)
            (fn this [& _]
              (println "Dumping dictionaries")
              (binding [*out* (-> (dict-file config ".is")
                                  java.io.FileWriter.)]
                
                (prn @dict-is)
                (.close *out*))
              (Thread/sleep (* 10 60000))
              (send-off *agent* this))))

(defn start-clojurebot [attrs additional-setup]
 (let [bot (pircbot attrs)]
   (dosync (commute *bots* assoc (:this bot) bot))
   (doto (:this bot)
     (.connect (:network bot))
     (.changeNick (:nick bot))
     (.joinChannel (:channel bot)))
   (additional-setup bot)
   bot))

(defmacro run-clojurebot [botname botattrs & additional-setup]
  `(start-clojurebot ~botattrs (fn [~botname] (do ~@additional-setup))))
