(cl:defpackage #:eclector.test
  (:use
   #:common-lisp
   #:alexandria
   #:fiveam)

  (:export
   #:error-case)

  (:export
   #:signals-printable)

  (:export
   #:run-tests))

(cl:in-package #:eclector.test)

;;; Main test suite and test entry point

(def-suite :eclector)

(defun run-tests ()
  (let ((*print-circle* t))
    (run! :eclector)))
