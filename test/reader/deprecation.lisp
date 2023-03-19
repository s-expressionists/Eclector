(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.deprecation
  :in :eclector.reader)

;;; Test customizing CALL-WITH-CURRENT-PACKAGE

(defclass with-current-package-client ()
  ())

(defmethod eclector.reader:call-with-current-package
    ((client with-current-package-client)
     (thunk t)
     (package-designator t))
  (let ((*package* (find-package '#:eclector.reader.test)))
    (funcall thunk)))

(defmethod eclector.reader:check-feature-expression
    ((client with-current-package-client)
     (feature-expression t))
  (labels ((check (expression)
             (typecase expression
               ((and symbol (not null))
                (is (eq (find-package '#:eclector.reader.test)
                        (symbol-package expression))))
               (cons
                (check (car expression))
                (check (cdr expression))))))
    (check feature-expression)))

(defmethod eclector.reader:evaluate-feature-expression
    ((client with-current-package-client)
     (feature-expression t))
  (eclector.reader:check-feature-expression client feature-expression)
  t)

(test call-with-current-package/customize
  "Test customizing the behavior of CALL-WITH-CURRENT-PACKAGE."
  (do-input-cases (input)
    (let ((eclector.reader:*client*
            (make-instance 'with-current-package-client)))
      (eclector.reader:read-from-string input))
    '(("#+foo       1 2")
      ("#+(bar baz) 1 2"))))
