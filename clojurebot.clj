;;
;; Thus spake the master programmer:
;;   "Though a program be but three lines long, someday it will have to be
;;   maintained."
;; 

(ns hiredman.clojurebot
    (:import (org.jibble.pircbot PircBot)))

(def nick "clojurebot")
(def channel "#clojure")
(def net "chat.us.freenode.net")

;; dictionaries for storing relationships
;; 'are' dict is not used right now.
(def dict-is (ref {}))
(def dict-are (ref {}))
(def url (ref {}))

(def url-regex #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

(defstruct junks :this :channel :sender :login :hostname :message)

(defn randth [se]
      (let [s (seq se)]
        (first (drop (rand-int (count se)) se))))

;; responses that can be randomly selected from
(def input-accepted ["In Ordnung" "Ik begrijp" "Alles klar" "Ok." "Roger." "You don't have to tell me twice." "Ack. Ack." "c'est bon!"])
(def befuddl ["Titim gan éirí ort." "No entiendo" "Извините?" "excusez-moi" "Excuse me?" "Huh?" "I don't understand." "Pardon?" "It's greek to me."])

(defn ok []
      (randth input-accepted))

(defn befuddled []
      (randth befuddl))

(defn strip-is [string]
      (.trim (.substring string (+ 3 (.indexOf string " is ")))))

(defn term [string]
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
      (async (.sendMessage this recv (.replace (str msg) \newline \ ))))

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

(defmulti define (fn [pojo term defi]
                     (if (and (@dict-is term) (re-find #"also" (:message pojo)))
                       :add
                       :new)))

(defmethod define :new [pojo term defi]
  (dosync
    (alter dict-is
           (fn [dict]
               (let [r (assoc dict (.trim term) (.trim defi))]
                 (when r (sendMsg (:this pojo) (who pojo) (ok)))
                 r)))))

(defmethod define :add [pojo term defi]
  (let [old (@dict-is term)
        ne (if (vector? old)
             (conj old defi)
             [old defi])]
    (dosync (alter dict-is (fn [dict] (let [r (assoc dict term ne)]
                                        (when r (sendMsg (:this pojo) (who pojo) (ok)))
                                        r))))))
 
(defn dispatch
      "this function does dispatch for responder"
      [pojo]
      (cond
        (doc-lookup? (:message pojo))
          :doc-lookup 
        (and (addressed? pojo) (re-find #"how much do you know?" (:message pojo)))
          :know
        (and (addressed? pojo) (re-find #"svn" (:message pojo)))
          :svn
        (and (addressed? pojo) (re-find #" is " (:message pojo)))
          :define-is
        (and (addressed? pojo) (re-find #" literal " (:message pojo)))
          :literal
        (re-find #"^\([\+ / - \*] [ 0-9]+\)" (:message pojo))
          :math
        (addressed? pojo) 
          :lookup
        (re-find url-regex (:message pojo))
          :url
        :else
          nil))

(defmulti #^{:docs "currently all messages are routed though this function"} responder dispatch)

(defmethod responder :math [pojo]
  (let [[op & num-strings] (re-seq #"[\+\/\*\-0-9]+" (:message pojo))
        nums (map #(.parseInt java.lang.Integer %) num-strings)]
    (sendMsg (:this pojo) (who pojo)
             (let [out (apply  (find-var (symbol "clojure.core" op)) nums)]
               (if (> out 4)
                 "*suffusion of yellow*"
                 out)))))

(defmethod responder :doc-lookup [pojo]
  (sendMsg (:this pojo)
           (who pojo)
           (symbol-to-var-doc (subs (:message pojo) 5 (dec (count (:message pojo)))))))

(defmethod responder :svn [pojo]
  (sendMsg (:this pojo)
           (who pojo)
           "svn co https://clojure.svn.sourceforge.net/svnroot/clojure clojure"))

(defmethod responder :define-is [pojo]
  (let [a (.trim (.replaceFirst (:message pojo) "^clojurebot:" " "))
        term (term a)
        defi (.replaceFirst (strip-is a) "^also " "")]
    (define pojo term defi)))

(defmethod responder :lookup [pojo]
  (let [msg (d?op (.trim (.replaceFirst (:message pojo) (str "^" nick ":") "")))
        result ((deref dict-is) msg)
        result (if (vector? result)
                 (randth result)
                 result)]
    (cond
      result
        (sendMsg (:this pojo) (who pojo)
                 (.replaceAll (if (re-find #"^<reply>" result)
                                (.trim (.replaceFirst (str result) "^<reply>" ""))
                                (str msg " is " result))
                              "#who"
                              (:sender pojo)))
      :else
        (sendMsg (:this pojo) (who pojo) (befuddled)))))

(defmethod responder :know [pojo]
  (sendMsg (:this pojo) (who pojo) (str "I know " (+ (count (deref dict-is)) (count (deref dict-are))) " things")))

(defmethod responder :url [pojo]
  (dosync (alter url (fn [url] 
                 (assoc url (re-find url-regex (:message pojo)) (java.util.Date.)))))
  (prn (:sender pojo) "> " (:message pojo)))

(defmethod responder :literal [pojo]
  (let [q (.replaceFirst (:message pojo) (str "^" nick ": literal ") "")]
    (prn q)))

(defn handleMessage [this channel sender login hostname message]
      (responder (struct junks this channel sender login
                         hostname message)))

(defn handlePrivateMessage [this sender login hostname message]
      (handleMessage this nil sender login hostname message))

(defn pircbot []
      (proxy [PircBot] []
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

(defn dump-thread []
      (.start (Thread.
                (fn []
                    (prn (str (java.util.Date.) " " :dump))
                    (dumpdicts)
                    (Thread/sleep 60000)
                    (recur)))))

      
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

(def *bot* (pircbot))
(.connect *bot* net)
(.changeNick *bot* nick)
(.joinChannel *bot* channel)
