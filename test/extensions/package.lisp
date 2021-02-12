(defpackage #:eclector.syntax-extensions.test
  (:use
   #:cl
   #:fiveam)

  (:export
   #:run-tests))

(in-package #:eclector.syntax-extensions.test)

(def-suite :eclector.extensions)

(defun run-tests ()
  (run! :eclector.extensions))
