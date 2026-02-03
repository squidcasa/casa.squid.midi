(ns casa.squid.midi.javax
  "javax.sound.midi based backend for casa.squid.midi"
  {:author "Arne Brasseur"}
  (:refer-clojure :exclude [send])
  (:require
   [casa.squid.midi :as midi]
   [clojure.string :as str])
  (:import
   (javax.sound.midi MidiDevice
                     MidiSystem
                     Receiver
                     Transmitter
                     MidiMessage
                     SysexMessage
                     MetaMessage
                     ShortMessage)))

(set! *warn-on-reflection* true)

(defprotocol Coercions
  (receiver ^Receiver [_])
  (transmitter ^Transmitter [_]))

(extend-protocol Coercions
  MidiDevice
  (receiver [this]
    (.getReceiver this))
  (transmitter [this]
    (.getTransmitter this))

  Transmitter
  (transmitter [this] this)

  Receiver
  (receiver [this] this))

(defn connect [from to]
  (.setReceiver (transmitter from) (receiver to)))

(defn disconnect [^Transmitter from]
  ;; We don't coerce here since we need the actual transmitter used in connect,
  ;; so call transmitter yourself if you want to be able to disconnect.
  (.setReceiver from nil))

(defn java-midi-message
  "Convert a midi message (byte or long array) into a MidiMessage"
  [msg]
  ;; Note: 0xFF can mean reset or meta, depending on if it's sent over the wire,
  ;; or read from a file. Java uses separate classes to represent these. We
  ;; always encode it as a reset ShortMessage
  (if (= :sysex-start (midi/event-type msg))
    (SysexMessage. msg (count msg))
    (ShortMessage. (midi/status msg)
                   (midi/data1 msg)
                   (midi/data2 msg))))

(defn send [to msg timestamp]
  (.send (receiver to)
         ^MidiMessage
         (cond
           (.isArray (class msg))      (java-midi-message msg)
           (vector? msg)               (java-midi-message (midi/message msg))
           (instance? MidiMessage msg) msg)
         timestamp))

(defn device-name [^MidiDevice d]
  (.getName (.getDeviceInfo d)))

(defn receivers [^MidiDevice d]
  (.getReceivers d))

(defn transmitters [^MidiDevice d]
  (.getTransmitters d))

(defn can-transmit? [^MidiDevice d]
  ;; Can be -1 for unlimited
  (not= 0 (.getMaxTransmitters d)))

(defn can-receive? [^MidiDevice d]
  ;; Can be -1 for unlimited
  (not= 0 (.getMaxReceivers d)))

(defn query-devices []
  (map #(MidiSystem/getMidiDevice %)
       (MidiSystem/getMidiDeviceInfo)))
(doseq [info (MidiSystem/getMidiDeviceInfo)]
  (let [device (MidiSystem/getMidiDevice info)]
    (println "---")
    (println "Name:       " (.getName info))
    (println "Vendor:     " (.getVendor info))
    (println "Description:" (.getDescription info))
    (println "Class:      " (.getName (class device)))))

(defn find-input-device [name]
  (first (filter (every-pred #(str/includes? (device-name %) name)
                             can-transmit?)
                 (query-devices))))

(defn find-output-device [name]
  (first (filter (every-pred #(str/includes? (device-name %) name)
                             can-receive?)
                 (query-devices))))

(defn receiver-callback
  "Create a javax.sound.midi.Receiver that delegates to a callback function.

  Callback receives [message millis] where message is a byte-array and millis is
  epoch-based timestamp."
  [f]
  (proxy [Receiver] []
    (send [^MidiMessage message ^long timestamp]
      ;; javax.sound.midi uses microseconds, convert to milliseconds
      (f (.getMessage message) (quot timestamp 1000)))
    (close [])))

(defonce ^:private callback->receiver (atom {}))

(defn add-receiver-impl
  "Register a MIDI receiver callback on a transmitting device/transmitter.

  Callback signature is [message millis], with message a byte-array, and millis
  an epoch-based timestamp."
  [port callback]
  (let [rcv (receiver-callback callback)
        tx (transmitter port)]
    (.setReceiver tx rcv)
    (swap! callback->receiver assoc callback {:receiver rcv :transmitter tx})))

(defn remove-receiver-impl
  "Remove a MIDI receiver callback, relies on object identity of the callback."
  [port callback]
  (when-let [{:keys [^Receiver receiver ^Transmitter transmitter]} (get @callback->receiver callback)]
    (.close receiver)
    (.close transmitter)
    (swap! callback->receiver dissoc callback)))

(defn write-impl
  "Write (send) a MIDI message to the given port.

  Offset is ignored for javax.sound.midi (messages are sent immediately)."
  [port message offset]
  (send port message -1))

(extend MidiDevice
  midi/MidiOps
  {:-add-receiver add-receiver-impl
   :-remove-receiver remove-receiver-impl
   :-write write-impl})

(extend Receiver
  midi/MidiOps
  {:-write write-impl})

(extend Transmitter
  midi/MidiOps
  {:-add-receiver add-receiver-impl
   :-remove-receiver remove-receiver-impl})
