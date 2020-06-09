(cl:defpackage #:eclector.test
  (:use
   #:common-lisp
   #:alexandria
   #:fiveam)

  (:export
   #:run-tests
   #:error-case)

  (:export
   #:signals-printable))

(cl:in-package #:eclector.test)

;;; Main test suite and test entry point

(def-suite :eclector)

(defun run-tests ()
  (run! :eclector))
