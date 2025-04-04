(cl:defpackage #:eclector.test
  (:use
   #:common-lisp
   #:alexandria
   #:fiveam)

  (:export
   #:equal*
   #:equalp*
   #:code-equal

   #:check-signals-error
   #:error-case)

  (:export
   #:gen-labels-and-references)

  (:export
   #:map-all-system-files
   #:map-all-system-expressions)

  (:export
   #:run-tests))

(cl:in-package #:eclector.test)

;;; Main test suite and test entry point

(def-suite :eclector)

(defun run-tests ()
  (let ((*print-circle* t))
    (run! :eclector)))
