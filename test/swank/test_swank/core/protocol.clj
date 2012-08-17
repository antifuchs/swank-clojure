(ns swank.test-swank.core.protocol
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream
                    StringWriter))
  (:use clojure.test
        swank.core.protocol))

(defn- read-msg [msg]
  (with-open [reader (ByteArrayInputStream. (.getBytes msg "utf-8"))]
    (read-swank-message reader)))

;; currently here until test-is
(deftest reading-messages ()
  (is (= (read-msg "0000017")                        7))
  (is (= (read-msg "000013(:keyword \"string\")")    '(:keyword "string")))
  (is (= (read-msg "000018(nested (list [vector]))") '(nested (list [vector])))))

(deftest cl-cljs-edge-cases ()
  (is (= (read-msg "000013(nested (list [t]))")      '(nested (list [true])))))

(defn- write-msg [form]
  (with-open [writer (ByteArrayOutputStream.)]
    (write-swank-message writer form)
    (.toString writer "utf-8")))

(deftest writing-messages
  (is (= (write-msg 9)                         "0000019"))
  (is (= (write-msg '(:keyword "string"))      "000013(:keyword \"string\")"))
  (is (= (write-msg '(nested (list [vector]))) "000018(nested (list [vector]))")))