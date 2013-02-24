(ns clojurebot.irc
  (:require [vespa.crabro :as vc]
            [vespa.protocols :as p]
            [clojure.tools.logging :as log])
  (:import (org.jibble.pircbot PircBot))
  (:gen-class))

(defn wall-hack-method [class-name name- params obj & args]
  (-> class-name (.getDeclaredMethod (name name-) (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(defn pircbot [server nick fun]
  (let [connector (if (coll? server)
                    (fn [conn]
                      (let [[server port pass] server]
                        (.connect conn server port pass)))
                    #(.connect % server))
        server (if (coll? server) (first server) server)
        conn (proxy [PircBot] []
               (onConnect []
                 (fun {:op :connect
                       :server server
                       :nick nick}))
               (onDisconnect []
                 (fun {:op :disconnect
                       :server server
                       :nick nick}))
               (onMessage [channel sender login hostname message]
                 (fun {:op :message
                       :channel channel
                       :sender sender
                       :login login
                       :hostname hostname
                       :message message
                       :server server}))
               (onAction [sender login hostname target action]
                 (fun {:op :action
                       :target target
                       :sender sender
                       :login login
                       :hostname hostname
                       :action action
                       :message action
                       :server server}))
               (onInvite [target-nick source-nick source-login source-hostname
                          channel]
                 (fun {:op :invite
                       :target-nick target-nick
                       :source-nick source-nick
                       :source-login source-login
                       :source-hostname source-hostname
                       :channel channel
                       :server server}))
               (onPrivateMessage [sender login hostname message]
                 (fun {:op :private-message
                       :sender sender
                       :login login
                       :hostname hostname
                       :message message
                       :server server}))
               (onJoin [channel sender login hostname]
                 (fun {:op :join
                       :sender sender
                       :login login
                       :hostname hostname
                       :channel channel
                       :server server}))
               (onPart [channel sender login hostname]
                 (fun {:op :part
                       :sender sender
                       :login login
                       :hostname hostname
                       :channel channel
                       :server server}))
               (onQuit [nick login hostname reason]
                 (fun {:op :quit
                       :nick nick
                       :login login
                       :hostname hostname
                       :reason reason
                       :server server}))
               (onVersion [nick login hostname target]
                 (fun {:op :version
                       :nick nick
                       :login login
                       :hostname hostname
                       :target target
                       :server server}))
               (close []
                 (.disconnect this)))]
    (wall-hack-method
     org.jibble.pircbot.PircBot :setName [String] conn nick)
    (connector conn)
    conn))

(defn -main [server nick queue-prefix]
  (let [mb (vc/message-bus)
        bot (pircbot server nick
                     (fn [m]
                       (log/debug m)
                       (vc/send-to mb (str queue-prefix ".in") m)))]
    (while true
      (try
        (let [m (p/receive-from mb (str queue-prefix ".out") identity)]
          (case (:op m)
            :everyone-I-see
            (vc/send-to mb
                        (:queue m)
                        (into {} (for [channel (.getChannels bot)]
                                   [channel (map (comp :nick bean) (.getUsers bot channel))])))
            :action
            (.sendAction bot (:target m) (:action m))
            :invite
            (.sendInvite bot (:nick m) (:channel m))
            :message
            (.sendMessage bot (:target m) (:message m))
            :notice
            (.sendNotice bot (:target m) (:notice m))))))))
