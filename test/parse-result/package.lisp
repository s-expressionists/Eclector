(cl:defpackage #:eclector.parse-result.test
  (:use
   #:common-lisp
   #:fiveam)

  (:import-from #:eclector.test
   #:signals-printable)

  (:export
   #:run-tests))

(cl:in-package #:eclector.parse-result.test)

(def-suite :eclector.parse-result
  :in :eclector)

(defun run-tests ()
  (run! :eclector.parse-result))
