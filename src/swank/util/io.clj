(ns swank.util.io
  (:use [swank util]
        [swank.util.concurrent thread])
  (:import [java.lang.String]
           [java.io StringWriter Reader PrintWriter]))

(defn read-bytes [input-stream n]
  (let [bytes (byte-array n)
        read-length (.read input-stream bytes)]
    (when (not= read-length n)
      (throw (java.io.EOFException. (format "Could not read 0x%x bytes, got 0x%x" n read-length))))
    bytes))

(defn call-on-flush-stream
  "Creates a stream that will call a given function when flushed."
  ([flushf]
     (let [closed? (atom false)
           #^PrintWriter stream
           (PrintWriter.
            (proxy [StringWriter] []
              (close [] (reset! closed? true))
              (flush []
                     (let [#^StringWriter me this
                           len (.. me getBuffer length)]
                       (when (> len 0)
                         (flushf (.. me getBuffer (substring 0 len)))
                         (.. me getBuffer (delete 0 len)))))))]
       (dothread
        (thread-set-name "Call-on-write Stream")
        (continuously
         (Thread/sleep 200)
         (when-not @closed?
           (.flush stream))))
       stream))
  {:tag PrintWriter})
