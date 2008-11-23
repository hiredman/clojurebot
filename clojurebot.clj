;;
;; Thus spake the master programmer:
;;   "Though a program be but three lines long, someday it will have to be
;;   maintained."
;; 

(ns hiredman.clojurebot
    (:import (org.jibble.pircbot PircBot)))

(def nick "clojurebot")
(def channel "#clojure")
(def net "chat.feenode.org")

;; dictionaries for storing relationships
;; 'are' dict is not used right now.
(def dict-is (ref {}))
(def dict-are (ref {}))

(def url-regex #"[A-Za-z]+://[^  ^/]+\.[^  ^/]+[^ ]+")

(defstruct junks :this :channel :sender :login :hostname :message)

;; responses that can be randomly selected from
(def response
     {:input-accepted ["Ok." "Roger." "You don't have to tell me twice." "Ack. Ack." "c'est bon!"]
      :befuddled ["Titim gan éirí ort." "excusez-moi" "Excuse me?" "Huh?" "I don't understand." "Pardon?" "It's greek to me."]})

(defn random-response
      "select a random response of the correct type"
      [type]
      ((response type) (rand-int (count (response type)))))

(defn ok []
      (random-response :input-accepted))
(defn befuddled []
      (random-response :befuddled))


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
      (let [x (:doc (meta (find-var (symbol "clojure.core" symb))))]
        (if x
          x
          (befuddled))))

(defmacro async
  "just do this, I don't care"
  [& x]
  `(send-off (agent nil) (fn [& _#] ~@x )))

(defn sendMsg
      "send a message to a recv, a recv is a channel name or a nick"
      [this recv msg]
      (async (.sendMessage this recv (.replace msg \newline \ ))))

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
        (re-find #"^\([\+ / - \*] [ 0-9]+\)" (:message pojo))
          :math
        (addressed? pojo) 
          :lookup
        (re-find url-regex (:message pojo))
          :url
        :else
          nil))

(defmulti responder dispatch)

(defmethod responder :math [pojo]
  ;(re-seq #"[\+\/\*\-0-9]+" "(- 1 2 3 4)")
  (prn :math))

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
        [term defi] (.split a " is ")]
    (dosync
      (alter dict-is
             (fn [dict]
                 (let [r (assoc dict (.trim term) (.trim defi))]
                   (when r
                     (sendMsg (:this pojo) (who pojo) (ok)))
                   r))))))

(defmethod responder :lookup [pojo]
  ; looks up message in dict
  (let [msg (d?op (.trim (.replaceFirst (:message pojo) "^clojurebot:" "")))]
    (cond
      ((deref dict-is) msg)
        (sendMsg (:this pojo)
                 (who pojo)
                 (.replaceAll (if (re-find #"^<reply>" ((deref dict-is) msg))
                                (.trim
                                  (.replaceFirst (str ((deref dict-is) msg)) "^<reply>" ""))
                                (str msg " is " ((deref dict-is) msg)))
                              "#who"
                              (:sender pojo)))
      :else
         (sendMsg (:this pojo) (who pojo) (befuddled)))))

(defmethod responder :know [pojo]
  (sendMsg (:this pojo) (who pojo) (str "I know " (+ (count (deref dict-is)) (count (deref dict-are))) " things")))

(defmethod responder :url [pojo]
  (prn (:sender pojo) "> " (:message pojo)))


(defn handleMessage [this channel sender login hostname message]
      (responder (struct junks this channel sender login
                         hostname message))

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
      
;; (.start (Thread. (fn []
;;              (loop []
;;                (dumpdicts)
;;                (.sleep Thread 600000)
;;                (recur)))))
;;  
;; (dosync 
;;   (ref-set dict-is
;; (eval (binding [*in* (-> "clojurebot.is"
;;                    java.io.File.
;;                    java.io.FileReader.
;;                    java.io.PushbackReader.)]
;;          (let [a (read)]
;;            (.close *in*)
;;            a))))
;; )
;; (update-proxy bot {'onMessage handleMessage
;;                    'onPrivateMessage handlePrivateMessage})

(def *bot* (pircbot))
(.connect *bot* net)
(.changeNick *bot* nick)
(.joinChannel *bot* channel)
