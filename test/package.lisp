(cl:defpackage #:eclector.test
  (:use
   #:common-lisp
   #:fiveam)

  (:export
   #:run-tests)

  (:export
   #:signals-printable))

(cl:in-package #:eclector.test)

;;; Main test suite and test entry point

(def-suite :eclector)

(defun run-tests ()
  (run! :eclector))

;;; Test utilities

(defmacro signals-printable (condition &body body)
  (let ((do-it (gensym "DO-IT")))
    `(flet ((,do-it () ,@body))
       (signals ,condition (,do-it))
       (handler-case (,do-it)
         (,condition (condition)
           (when (typep condition
                        '(and reader-error
                          (not eclector.reader:backquote-condition)))
             (is (not (null (stream-error-stream condition)))))
           (is (not (string= "" (princ-to-string condition)))))))))
