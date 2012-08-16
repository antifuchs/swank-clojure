;;; This code has been placed in the Public Domain.  All warranties are disclaimed.
(ns #^{:doc "Pass remote calls and responses between lisp systems using the swank-rpc protocol."
       :author "Terje Norderhaug <terje@in-progress.com>"}
  swank.rpc
  (:use (swank util)
        (swank.util io))
  (:import (java.io InputStream OutputStream Writer)))

;; ERROR HANDLING

(def swank-protocol-error (Exception. "Swank protocol error."))

;; LOGGING

(def log-events false)

(def log-output nil)

(defn log-event [format-string & args]
  (when log-events
    (let [#^Writer out (or log-output *out*)]
      (.write out #^String (apply format format-string args))
      (.flush out))
    nil))

;; INPUT

(defn- read-form [str]
  "Read a form that conforms to the swank rpc protocol"
  (read-string str))

(def transport-encoding "utf-8")

(defn- read-packet
  ([#^InputStream input-stream]
   (let [len (Integer/parseInt (String. (read-bytes input-stream 6) transport-encoding) 16)]
     (String. (read-bytes input-stream len) transport-encoding))))

(defn decode-message
   "Read an rpc message encoded using the swank rpc protocol."
  ([#^InputStream rdr]
    (let [packet (read-packet rdr)]
       (log-event "READ: %s\n" packet)
       (try
        (read-form packet)
        (catch Exception e
               (list :reader-error packet e))))))

; (with-open [rdr (StringReader. "00001f(swank:a 123 (%b% (t nil) \"c\"))")] (decode-message rdr))


;; OUTPUT

(defmulti print-object (fn [x writer] (type x)))

(defmethod print-object :default [o, #^Writer w]
  (print-method o w))

(defmethod print-object Boolean [o, #^Writer w]
  (.write w (if o "t" "nil")))

(defmethod print-object String [#^String s, #^Writer w]
  (let [char-escape-string {\" "\\\""
                            \\  "\\\\"}]
    (do (.append w \")
        (dotimes [n (count s)]
          (let [c (.charAt s n)
                e (char-escape-string c)]
            (if e (.write w #^String e) (.append w c))))
        (.append w \"))
    nil))

(defmethod print-object clojure.lang.ISeq [o, #^Writer w]
  (.write w "(")
  (print-object (first o) w)
  (doseq [item (rest o)]
    (.write w " ")
    (print-object item w))
  (.write w ")"))

(defn- write-form
  ([#^Writer writer message]
    (print-object message writer)))

(defn- write-packet
  ([#^OutputStream stream #^String str]
   (let [bytes (.getBytes str transport-encoding)
         len (alength bytes)
         len-bytes (.getBytes (format "%06x" len) transport-encoding)]
    (doto stream
          (.write len-bytes)
          (.write bytes)
          (.flush)))))

(defn encode-message
  "Write an rpc message encoded using the swank rpc protocol."
  ([#^OutputStream stream message]
     (let [str (with-out-str
                  (write-form *out* message)) ]
       (log-event "WRITE: %s\n" str)
       (write-packet stream str))))

; (with-out-str (encode-message *out* "hello"))
; (with-out-str (encode-message *out* '(a 123 (swank:b (true false) "c"))))


;; DISPATCH

(defonce rpc-fn-map {})

(defn register-dispatch
  ([name fn]
    (register-dispatch name fn #'rpc-fn-map))
  ([name fn fn-map]
    (alter-var-root fn-map assoc name fn)))

(defn dispatch-message
  ([message fn-map]
    (let [operation (first message)
          operands (rest message)
          fn (fn-map operation)]
        (assert fn)
        (apply fn operands)))
  ([message]
   (dispatch-message message rpc-fn-map)))
