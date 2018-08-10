(cl:defpackage #:eclector.test
  (:use
   #:common-lisp
   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:eclector.test)

(def-suite :eclector)

(defun run-tests ()
  (run! :eclector))
