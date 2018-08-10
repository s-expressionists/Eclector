(cl:defpackage #:eclector.reader.test
  (:use
   #:common-lisp
   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:eclector.reader.test)

(def-suite :eclector.reader
    :in :eclector)

(defun run-tests ()
  (run! :eclector.reader))
