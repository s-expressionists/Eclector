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

;;; Test utilities

(defun %expected-condition-type (condition-type)
  (case condition-type
    (eclector.reader:feature-expression-type-error
     'eclector.reader::feature-expression-type-error/reader)
    (eclector.reader:single-feature-expected
     'eclector.reader::single-feature-expected/reader)
    (t
     condition-type)))

(defun %signals-printable (condition-type thunk)
  (handler-bind
      ((condition (lambda (condition)
                    (when (typep condition condition-type)
                      ;; The test should specify the exact expected
                      ;; condition type, not some supertype.
                      (is (type= (%expected-condition-type condition-type)
                                 (type-of condition)))
                      (when (typep condition
                                   `(and stream-error
                                         (not (or eclector.reader:unquote-splicing-in-dotted-list
                                                  eclector.reader:unquote-splicing-at-top))))
                        (is (not (null (stream-error-stream condition)))))
                      (is (not (string= "" (princ-to-string condition))))
                      (return-from %signals-printable)))))
    (funcall thunk)))

(defmacro signals-printable (condition &body body)
  (let ((do-it (gensym "DO-IT")))
    `(flet ((,do-it () ,@body))
       (signals ,condition (,do-it))
       (%signals-printable ',condition #',do-it))))

(defun handle-expected-error (expected thunk)
  (macrolet ((cases ()
               (flet ((condition-names ()
                        (let ((result '()))
                          (do-external-symbols (symbol '#:eclector.reader)
                            (when (and (find-class symbol nil)
                                       (subtypep symbol 'condition))
                              (push symbol result)))
                          result))
                      (do-type (type)
                        `(,type
                          (signals ,type (funcall thunk))
                          (%signals-printable ',type thunk)
                          t)))
                 `(case expected
                    ,@(map 'list #'do-type (condition-names))
                    (t nil)))))
    (cases)))

(defmacro error-case (expression &body clauses)
  (let* ((error-clause (find 'error clauses :key #'first))
         (error-body (rest error-clause))
         (other-clauses (remove 'error clauses :key #'first)))
    (once-only (expression)
     `(or (handle-expected-error ,expression (lambda () ,@error-body))
          (case ,expression
            ,@other-clauses)))))
