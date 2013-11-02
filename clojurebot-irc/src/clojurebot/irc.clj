;; a slight rewrite of conduit-irc
(ns clojurebot.irc
  (:require [clj-http.client :as http]
            [conduit.core :refer :all]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log])
  (:import (java.util.concurrent LinkedBlockingQueue)
           (java.io Closeable)
           (clojure.lang IDeref
                         Named)))

(defn url [bits]
  (apply str (interpose \/ bits)))

(def ^{:dynamic true} *pircbot* nil)

(defn- reply-fn [f]
  (partial (fn irc-reply-fn [f value]
             (let [[[new-value] new-f] (f value)]
               [[] (partial irc-reply-fn new-f)]))
           f))

(defprotocol IRCBot
  (get-channels [b])
  (join-channel [b channel])
  (-send-message [b recipient line])
  (-send-action [b recipient line])
  (-send-notice [b recipient line]))

(defn declare-joined [channel]
  (when-not (contains? (set (get-channels *pircbot*)) channel)
    (join-channel *pircbot* channel)))

(defn target-type [msg recipient]
  (if (.startsWith recipient "#")
    :channel
    :privmsg))

(defmacro defirc [fn-name method-name]
  `(do
     (defmulti ~fn-name target-type)
     (defmethod ~fn-name :channel [msg# recipient#]
       (declare-joined recipient#)
       (doseq [line# (.split msg# "\n")]
         (~method-name *pircbot* recipient# line#)))
     (defmethod ~fn-name :privmsg [msg# recipient#]
       (doseq [line# (.split msg# "\n")]
         (~method-name *pircbot* recipient# line#)))))

(defirc send-message -send-message)

(defirc send-action -send-action)

(defirc send-notice -send-notice)

(defn pircbot [server nick]
  (let [mq (LinkedBlockingQueue.)
        bid (if (coll? server)
              (let [[server port pass] server
                    [bid] (for [bot (edn/read-string
                                     (:body (http/get (url))))
                                :when (= server (:server bot))
                                :when (= port (:port bot))
                                :when (= nick (:nick bot))]
                            (:com.thelastcitadel.irc/bid bot))]
                (or
                 bid
                 (:body (http/post (url) {:form-params {:server server
                                                        :nick nick
                                                        :port port
                                                        :password pass}}))))
              (let [[bid] (for [bot (edn/read-string
                                     (:body (http/get (url))))
                                :when (= server (:server bot))
                                :when (= nick (:nick bot))]
                            (:com.thelastcitadel.irc/bid bot))]
                (or bid
                    (http/post (url) {:form-params {:server server
                                                    :nick nick}}))))
        fut (future
              (while true
                (Thread/sleep 1000)
                (try
                  (doseq [[eid event] (edn/read-string (:body (http/get (url bid "events"))))]
                    (.put mq [nick [(:type event) event]])
                    (http/delete (url bid "events" bid)))
                  (catch Exception e
                    (log/error e "error")))))
        server (if (coll? server) (first server) server)
        conn (reify
               IRCBot
               (get-channels [b]
                 (edn/read-string (:body (http/get (url bid "channels")))))
               (join-channel [b channel]
                 ;; TODO: url encode channel
                 (http/post (url bid "channel" channel)))
               Named
               (getName [_]
                 nick)
               IDeref
               (deref [_]
                 mq)
               Closeable
               (close [_]
                 (try
                   (future-cancel fut)
                   (finally
                     (http/delete (url bid))))))]
    conn))

(defn a-irc [nick proc]
  (let [id nick]
    (assoc proc
      :type :irc
      :parts (assoc (:parts proc)
               id {:type :irc
                   id (reply-fn (:reply proc))}))))

(defn join [channels]
  (doseq [channel channels]
    (println channel)
    (join-channel *pircbot* channel)))

(defn irc-run
  "start a single thread executing a proc"
  [proc & [channel-or-exception-handler & channels]]
  (let [funs (get-in proc [:parts (name *pircbot*)])]
    (join
     (if (fn? channel-or-exception-handler)
       channels
       (conj channels channel-or-exception-handler)))
    (letfn [(next-msg [Q]
              (fn next-msg-inner [_]
                [[(.take Q)] next-msg-inner]))
            (handle-msg [fun msg]
              (try
                (let [[_ new-fn] (fun msg)]
                  [[] (partial handle-msg new-fn)])
                (catch Exception e
                  (if (fn? channel-or-exception-handler)
                    (channel-or-exception-handler e)
                    (.printStackTrace e))
                  [[] fun])))
            (run []
              (->> [(next-msg @*pircbot*)
                    (partial handle-msg (partial select-fn funs))]
                   (reduce comp-fn)
                   (a-run)
                   (dorun)))]
      (run))))

(comment

  (with-open [p (pircbot "irc.freenode.net" "conduitbot11")]
    (binding [*pircbot* p]
      (irc-run
       (a-irc "conduitbot11"
              (a-arr
               (fn [[t m]]
                 (println t (dissoc m :bot)))))
       "#clojurebot")))

  )
