;; DEPENDS smack
(ns hiredman.clojurebot.xmpp
  (:require [hiredman.clojurebot.core :as core])
  (:import (org.jivesoftware.smack XMPPConnection ConnectionConfiguration
                                   RosterListener PrivacyListManager
                                   PacketListener MessageListener)
           (org.jivesoftware.smack.packet Message Message$Type IQ IQ$Type Presence Presence$Type)
           (org.jivesoftware.smackx.muc MultiUserChat)
           (org.jivesoftware.smack.filter PacketFilter MessageTypeFilter OrFilter)
           (org.jivesoftware.smackx.packet VCard)
           (org.jivesoftware.smackx.filetransfer FileTransferManager)
           (java.io File)))


(defn connect [jid pass]
  (doto (XMPPConnection. (last (.split jid "@")))
    .connect
    (.login (first (.split jid "@")) pass)))

(defn muc [connection room]
  (doto (MultiUserChat. connection room)
    (.join (first (.split (.getUser connection) "@")))))

(declare msg->junks)

(defn packet-processor [bot p]
  (when (not (nil? (.getBody p)))
    (trampoline core/responder
      (assoc bot :xmpp-message p)
      (let [g (msg->junks bot p)]
        (vary-meta g assoc :addressed? (core/addressed? bot g)
                   :xmpp-message p)))))


(defn msg->junks [bot msg]
    (struct core/junks
      (when (= (.getType msg) (Message$Type/fromString "groupchat"))
        (muc (:con bot) (first (.split (.getFrom msg) "/"))))
        (first (.split (.getFrom msg) "@"))
        nil
        nil
        (.getBody msg)))

(defn packet-listener [bot]
  (proxy [PacketListener] []
    (processPacket [p]
                   (try
                     (binding [core/random-person #(do % "foo")]
                       (packet-processor bot p))
                     (catch Exception e
                       (prn e))))))

(defn clone-message [msg]
  (doto (Message.)
    (.setSubject (.getSubject msg))
    (.setThread (.getThread msg))
    (.setType (.getType msg))))

(defmethod core/new-send-out :xmpp [bot msg-type r w]
  (when-not (= w "")
    (if (= (.getType (:xmpp-message bot)) (Message$Type/fromString "groupchat"))
      (.sendMessage (:channel r r) w)
      (.sendPacket (:con bot)
                   (doto (clone-message (:xmpp-message bot))
                    (.setFrom (.getTo (:xmpp-message bot)))
                    (.setTo (.getFrom (:xmpp-message bot)))
                    (.setBody w))))))

;(def chat (muc con "clojure@conference.thelastcitadel.com"))

(defn connect-to-muc [bot mu]
  (muc (:xmpp-connection bot) mu)
  bot)

(defn setup-listener [bot]
  (.addPacketListener (:xmpp-connection bot)
                      (packet-listener #^{:type :xmpp} {:con (:xmpp-connection bot) :nick "clojurebot"})
                      (OrFilter.
                        (MessageTypeFilter. (Message$Type/fromString "groupchat"))
                        (MessageTypeFilter. (Message$Type/fromString "chat"))))
  bot)
