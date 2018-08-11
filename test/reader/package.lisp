(cl:defpackage #:eclector.reader.test
  (:use
   #:common-lisp
   #:fiveam)

  (:import-from #:eclector.test
   #:signals-printable)

  (:export
   #:run-tests))

(cl:in-package #:eclector.reader.test)

;;; Main test suite and test entry point

(def-suite :eclector.reader
    :in :eclector)

(defun run-tests ()
  (run! :eclector.reader))
