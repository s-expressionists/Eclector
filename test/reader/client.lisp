(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

;;; Test customizing EVALUATE-EXPRESSION

(defclass evaluate-expression-client ()
  ())

(defmethod eclector.reader:evaluate-expression
    ((client evaluate-expression-client) (expression (eql 1)))
  (error "foo"))

(defmethod eclector.reader:evaluate-expression
    ((client evaluate-expression-client) (expression t))
  nil)

(test evaluate-expression/customize
  "Test customizing the behavior of EVALUATE-EXPRESSION."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected) input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((eclector.reader:*client*
                               (make-instance 'evaluate-expression-client)))
                         (eclector.reader:read stream)))))
              (case expected
                (eclector.reader:read-time-evaluation-error
                 (signals eclector.reader:read-time-evaluation-error
                   (do-it)))
                (t
                 (is (equal expected (do-it))))))))
        '(;; Errors
          ("(1 #.1 3)"          eclector.reader:read-time-evaluation-error)
          ;; No errors
          ("(1 #.2 3)"          (1 nil 3))
          ("(1 #.(list #.2) 3)" (1 nil 3)))))
