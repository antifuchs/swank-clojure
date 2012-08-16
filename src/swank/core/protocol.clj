(ns swank.core.protocol
  (:use (swank util)
        (swank.util io))
  (:require swank.rpc clojure.string)
  (:import (java.io InputStream OutputStream)))

(defn- fix-namespace-on-symbol [the-sym]
  (let [symname (name the-sym)
        parts (clojure.string/split symname #"::?" 2)]
    (apply symbol parts)))

(defn- fix-namespace
  "Changes the namespace of a function call from pkg:fn to ns/fn. If
   no pkg exists, then nothing is done."
  ([form] (if (= (first form) :emacs-rex)
              `(:emacs-rex (~(fix-namespace-on-symbol (first (second form)))
                             ~@(rest (first (rest form))))
                           ~@(rest (rest form)))
              form)))

(defn write-swank-message
  "Given a `writer' (java.io.Writer) and a `message' (typically an
   sexp), encode the message according to the swank protocol and
   write the message into the writer."
  ([#^java.io.OutputStream output-stream message]
     (swank.rpc/encode-message output-stream message))
  {:tag String})

(def read-fail-exception (Exception. "Error reading swank message"))

(defn read-swank-message
  "Given a `reader' (java.io.Reader), read the message as a clojure
   form (typically a sexp). This method will block until a message is
   completely transfered.

   Note: This function will do some amount of Common Lisp -> clojure
   conversions. This is due to the fact that several slime functions
   like to treat everything it's talking to as a common lisp
   implementation.
     - If an :emacs-rex form is received and the first form contains a
       common lisp package designation, this will convert it to use a
       clojure designation.
     - t will be converted to true

   See also `write-swank-message'."
  ([#^java.io.InputStream input-stream]
   (let [form (swank.rpc/decode-message input-stream)]
     (if (seq? form)
         (deep-replace {'t true} (fix-namespace form))
         form))))
