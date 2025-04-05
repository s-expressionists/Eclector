(cl:defpackage #:eclector.concrete-syntax-tree.test
  (:use
   #:common-lisp
   #:fiveam)

  (:import-from #:eclector.test
   #:equal* #:equalp*

   #:do-input-cases        #:expect
   #:do-stream-input-cases #:with-stream
   #:error-case

   #:gen-labels-and-references)

  (:export
   #:run-tests))

(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite :eclector.concrete-syntax-tree)

(defun run-tests ()
  (let ((*print-circle* t))
    (run! :eclector.concrete-syntax-tree)))
