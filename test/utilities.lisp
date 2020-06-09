(cl:in-package #:eclector.test)

;;; Processing test cases

(defun wrap-in-expect (input-var form)
  `(macrolet ((expect (label test)
                `(is ,test
                     ,(format nil "For input ~~S, expected ~A ~~S but got ~~S."
                              label)
                     ,',input-var ,(second test) ,(third test))))
     ,form))

(defmacro do-input-cases ((input-and-length-vars &rest lambda-list) form cases)
  (destructuring-bind (input-var &optional length-var)
      (alexandria:ensure-list input-and-length-vars)
    (alexandria:with-unique-names (parameter)
      `(mapc (lambda (,parameter)
               (let* ((,input-var (format nil (first ,parameter)))
                      ,@(when length-var
                          `((,length-var (length ,input-var)))))
                 (destructuring-bind (,@lambda-list) (rest ,parameter)
                   ,(wrap-in-expect input-var form))))
             ,cases))))

(defmacro do-stream-input-cases (((&optional length-var) &rest lambda-list)
                                 form cases)
  (alexandria:with-unique-names (parameter input)
    `(mapc (lambda (,parameter)
             (let* ((,input (format nil (first ,parameter)))
                    ,@(when length-var
                        `((,length-var (length ,input)))))
               (destructuring-bind (,@lambda-list) (rest ,parameter)
                 (macrolet ((with-stream ((stream-var) &body body)
                              `(with-input-from-string (,stream-var ,',input)
                                 (let ((values (multiple-value-list
                                                (progn ,@body))))
                                   (multiple-value-call #'values
                                     (values-list values)
                                     (file-position ,stream-var))))))
                   ,(wrap-in-expect input form)))))
           ,cases)))

;;; Checking expected errors

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
