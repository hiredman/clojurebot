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


;java -server -Djava.security.manager -ms16m -mx64m -Xss128m

(ns hiredman.clojurebot
    (:import (org.jibble.pircbot PircBot)
             (java.util.concurrent FutureTask TimeUnit TimeoutException))
    (:require [hiredman.sandbox :as s]))

;; set up the namespace for the sandbox
(binding [*ns* (create-ns 'foo)]
         (clojure.core/refer 'clojure.core)
         (import '(java.util Date)))

(def *nick* "clojurebot")
(def *channel* "#clojure")
(def *net* "chat.us.freenode.net")

(def *bot*) ;this will be the bot object

(def start-date (java.util.Date.))

;; dictionaries for storing relationships
;; 'are' dict is not used right now.
(def dict-is (ref {}))
(def dict-are (ref {}))
(def users (ref {}))

;url is for storing urls, must figure out something to do with this
(def url (ref {}))

(def svn-rev-cache (ref []))

(def url-regex #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

;; this struct is used to pass around messages
(defstruct junks :this :channel :sender :login :hostname :message)

(defn randth
      "random item from sequence"
      [se]
      (let [s (seq se)]
        (first (drop (rand-int (count se)) se))))

;; responses that can be randomly selected from
(def input-accepted ["'Sea, mhuise." "In Ordnung" "Ik begrijp" "Alles klar" "Ok." "Roger." "You don't have to tell me twice." "Ack. Ack." "c'est bon!"])
(def befuddl ["Titim gan éirí ort." "Gabh mo leithscéal?" "No entiendo"  "excusez-moi" "Excuse me?" "Huh?" "I don't understand." "Pardon?" "It's greek to me."])

(defn ok []
      (randth input-accepted))

(defn befuddled []
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

(defn sendMsg
      "send a message to a recv, a recv is a channel name or a nick"
      [this recv msg]
      (.sendMessage this recv (.replace (str msg) \newline \ )))

(defmacro sendMsg-who [pojo msg]
  `(sendMsg (:this ~pojo) (who ~pojo) ~msg))

(defn cache-svn-rev
      "puts an svn rev into the cache"
      [rev]
      (dosync (commute svn-rev-cache conj rev)))


(defn term-lists
      "generates permutions of the words in string"
      [msg]
      (let [x (re-seq #"\w+" msg)
            ignore #(not (contains? #{"a" "where" "what" "is" "who" "are" (str *nick* ": ")} %))]
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
      [message]
      (rlookup (term-lists message)))

(defn fuzzy-key-lookup
      "look up based on match part of a term"
      [term]
      (randth (filter #(when (> (.lastIndexOf % term) -1) true) (keys @dict-is))))

(defn who
      "am I talking to someonein a privmsg, or in a channel?"
      [pojo]
      (if (:channel pojo)
        (:channel pojo)
        (:sender pojo)))

(defn addressed?
      "is this message prefixed with clojurebot: "
      [pojo]
      (when (or (re-find #"^clojurebot:" (:message pojo)) (nil? (:channel pojo)))
        pojo))

;;; SVN
(def svn-command "svn -v --xml --limit 5 log  http://clojure.googlecode.com/svn/")

(defn svn-summaries
      "takes output of clojure.xml/parse on svn's xml log, returns
      a vector of [rev-number commit-message]"
      [tag-map]
      (map (fn [x]
               [(Integer/parseInt (:revision (:attrs x)))
                (first
                  (:content
                    (first
                      (filter #(= (:tag %) :msg)
                              (:content x)))))])
           (:content tag-map)))

(defn get-last-svn-rev []
      (Integer/parseInt (@dict-is "latest")))

(defn filter-newer-svn-revs [revs]
      (filter #(> (first %) (get-last-svn-rev))
              revs))

(defn send-svn-revs [revs]
      (dorun
        (map #(sendMsg *bot*
                        channel
                        (str "svn rev " (first %) "; " (last %)))
             revs)))


(defn svn-message
      "takes a seq of vectors containing [rev msg]
      sends out messages about new revs. updates \"latest\" 
      to latest rev"
      [summaries]
      (let [newrevs (filter-newer-svn-revs (reverse summaries))]
        (when newrevs
          (do
            (send-svn-revs newrevs)
            (dosync
              (commute dict-is
                       assoc
                       "latest"
                       (str (first (first summaries)))))
            ;don't want to see the whole hash in the repl
            nil))))

(defn svn-xml-stream
      "get the xml stream from svn"
      [cmd]
      (.getInputStream (.. Runtime getRuntime (exec cmd))))

(defn svn-notifier-thread []
      (send-off (agent nil)
                (fn this [& _]
                    (let [m (svn-summaries (clojure.xml/parse (svn-xml-stream svn-command)))]
                      (svn-message m)
                      (map cache-svn-rev m))
                    (Thread/sleep (* 5 60000))
                    (send-off *agent* this))))

;(svn-message (svn-summaries (clojure.xml/parse (svn-xml-stream svn-command))))
 

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

(defn seen [who doing what & stuff]
      (dosync
        (commute users assoc who [nil (str (name doing) what)])))
 
(defn dispatch
      "this function does dispatch for responder"
      [pojo]
      (cond
        (doc-lookup? (:message pojo))
          :doc-lookup 
        (re-find #"^,\(" (:message pojo))
          :code-sandbox
        (and (addressed? pojo) (re-find #"how much do you know?" (:message pojo)))
          :know
        (and (addressed? pojo) (re-find #" is " (:message pojo))  (not= \? (last (:message pojo))))
          :define-is
        (and (addressed? pojo) (re-find #" literal " (:message pojo)))
          :literal
        (re-find #"^\([\+ / \- \*] [ 0-9]+\)" (:message pojo))
          :math
        (re-find #"^svn rev [0-9]+$" (:message pojo))
          :svn-rev-lookup
        (addressed? pojo) 
          :lookup
        (re-find url-regex (:message pojo))
          :url
        :else
          nil))

(defmulti #^{:doc "currently all messages are routed though this function"} responder dispatch)

(defn find-or-create-ns [n]
      (if-let [s (find-ns n)] s (create-ns n)))

(defmethod responder :code-sandbox [pojo]
  (println (str (:sender pojo) " " (:message pojo)))
  (let [result (s/box (:message pojo))]
    (if (vector? result)
      (dorun (map #(sendMsg-who pojo (if (> (count %) 300)
                                       (reduce str (concat (take 300%) '(" ...")))
                                       %))
                  result))
      (sendMsg-who pojo result))))


(defmethod responder :math [pojo]
  (let [[op & num-strings] (re-seq #"[\+\/\*\-0-9]+" (:message pojo))
        nums (map #(Integer/parseInt %) num-strings)]
    (sendMsg-who pojo
                 (let [out (apply  (find-var (symbol "clojure.core" op)) nums)]
                   (if (> out 4)
                     "*suffusion of yellow*"
                     out)))))

(defmethod responder :doc-lookup [pojo]
  (sendMsg-who pojo
               (symbol-to-var-doc (subs (:message pojo)
                                        5
                                        (dec (count (:message pojo)))))))
(defn remove-from-beginning
  "return a string with the concatenation of the given chunks removed if it is
   found at the start of the string"
  [stringi & chunks]
  (.replaceFirst string (apply str "^" chunks) ""))

(defmethod responder :define-is [pojo]
  (let [a (.trim (remove-from-beginning (:message pojo) *nick* ":"))
        term (term a)
        x (strip-is a)
        defi (remove-from-beginning x "also ")]
    (if (re-find #"^also " x)
      (is term defi)
      (is! term defi))
    (sendMsg-who pojo (ok))))

(defn prep-reply [sender term defi]
      (.replaceAll (if (re-find #"^<reply>" defi)
                     (.trim (remove-from-beginning (str defi) "<reply>"))
                     (str term " is " defi))
                   "#who"
                   sender))

(defmethod responder :lookup [pojo]
  (let [msg (d?op (.trim (remove-from-beginning (:message pojo) *nick* ":")))
        result (what-is msg)]
    (cond
      result,
        (sendMsg-who pojo
                     (.replaceAll (if (re-find #"^<reply>" result)
                                    (.trim (remove-from-beginning (str result) "<reply>"))
                                    (str msg " is " result))
                                  "#who"
                                  (:sender pojo)))

      (fuzzy-lookup msg),
        (let [x (fuzzy-lookup msg)
              r (what-is x)]
          (sendMsg-who pojo (prep-reply (:sender pojo) term r)))

      (fuzzy-key-lookup msg),
        (let [term (fuzzy-key-lookup msg)
              defi (what-is term)]
          (sendMsg-who pojo (prep-reply (:sender pojo) term defi)))

      :else,
        (sendMsg-who pojo (befuddled)))))


(defmethod responder :know [pojo]
  (sendMsg-who pojo (str "I know " (+ (count (deref dict-is)) (count (deref dict-are))) " things")))

(defmethod responder :url [pojo]
  (dosync (commute url
                   assoc
                   (re-find url-regex (:message pojo)) (java.util.Date.)))
  (prn (str (:sender pojo) ", " (:message pojo))))

(defmethod responder :literal [pojo]
  (let [q (remove-from-beginning (:message pojo) *nick* ": literal ")]
    (prn q)))

(defmethod responder :svn-rev-lookup [pojo]
  (let [r (Integer/parseInt (re-find #"[0-9]+" (:message pojo)))
        t (filter #(= (first %) r)  @svn-rev-cache)]
    (if (not= 0 (count t))
      (send-svn-revs t)
      (let [cmd (.replace svn-command "--limit 5" (str "-r " r))
            b (svn-summaries (clojure.xml/parse (svn-xml-stream cmd)))]
        (do
          (send-svn-revs b)
          (dorun (map cache-svn-rev b)))))))

(defn user-watch []
      (let [cur (count (.getUsers *bot* "#clojure"))
            pre (Integer/parseInt (what-is "max people"))]
        (when (> cur pre)
          (is! "max people" (str cur)))))


(defn handleMessage [this channel sender login hostname message]
      (when channel
        (seen sender :saying message))
      (responder (struct junks this channel sender login
                         hostname message)))

(defn handlePrivateMessage [this sender login hostname message]
      (handleMessage this nil sender login hostname message))

(defn pircbot []
      (proxy [PircBot] []
             (onJoin [channel sender login hostname]
                     (user-watch))
             (onMessage [channel sender login hostname message]
                        (handleMessage this channel sender login hostname message))
             (onPrivateMessage [sender login hostname message]
                        (handlePrivateMessage this  sender login hostname message))))

(defn dumpdicts []
      (map (fn [[rel rels]]
               (binding [*out* (-> (str "clojurebot." rel)
                                   java.io.File.
                                   java.io.FileWriter.)]
                        (prn @rels)
                        (.close *out*)))
           [["is" dict-is] ["are" dict-are]]))

   
(defn load-dicts []
      (dosync
        (ref-set dict-is
                 (eval
                   (binding [*in* (-> "clojurebot.is"
                                          java.io.File.
                                          java.io.FileReader.
                                          java.io.PushbackReader.)]
                                (let [a (read)]
                                  (.close *in*)
                                  a))))))

(defn dump-thread []
      (send-off (agent nil)
                (fn this [& _]
                    (binding [*out* (-> "clojurebot.is"
                                        java.io.File.
                                        java.io.FileWriter.)]
                             (prn @dict-is)
                             (.close *out*))
                    (Thread/sleep (* 10 60000))
                    (send-off *agent* this))))

(def *bot* (pircbot))
(s/enable-security-manager)
(.connect *bot* *net*)
(.changeNick *bot* *nick*)
(.joinChannel *bot* *channel*)
(load-dicts)
(svn-notifier-thread)
(dump-thread)
