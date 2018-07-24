(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

;;; Test customizing FIND-CHARACTER

(defclass find-character-client ()
  ())

(defmethod eclector.reader:find-character
    ((client find-character-client) (name t))
  (if (string= name "NO_SUCH_CHARACTER")
      nil
      #\a))

(test find-character/customize
  "Test customizing the behavior of FIND-CHARACTER."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected) input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((eclector.reader:*client*
                               (make-instance 'find-character-client)))
                         (eclector.reader:read stream)))))
              (case expected
                (eclector.reader:unknown-character-name
                 (signals eclector.reader:unknown-character-name
                   (do-it)))
                (t
                 (is (equal expected (do-it))))))))
        '(;; Errors
          ("#\\no_such_character" eclector.reader:unknown-character-name)
          ("#\\NO_SUCH_CHARACTER" eclector.reader:unknown-character-name)

          ;; Single character
          ("#\\a"                 #\a)
          ("#\\A"                 #\A)
          ("#\\b"                 #\b)
          ("#\\B"                 #\B)

          ;; Multiple characters
          ("#\\name"              #\a)
          ("#\\Name"              #\a)
          ("#\\NAME"              #\a))))

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

;;; Test customizing {CHECK,EVALUATE}-FEATURE-EXPRESSION

(defclass feature-expression-client ()
  ())

(defmethod eclector.reader:check-feature-expression
    ((client feature-expression-client)
     (feature-expression t))
  (or (typep feature-expression '(cons (eql :version-at-least) (cons string null)))
      (call-next-method)))

(defmethod eclector.reader:evaluate-feature-expression
    ((client feature-expression-client)
     (feature-expression (eql :my-special-feature)))
  (eclector.reader:check-feature-expression client feature-expression)
  t)

(defmethod eclector.reader:evaluate-feature-expression
    ((client feature-expression-client)
     (feature-expression cons))
  (case (first feature-expression)
    (:not
     (eclector.reader:check-feature-expression client feature-expression)
     (eclector.reader:evaluate-feature-expression
      client (second feature-expression)))
    (:version-at-least
     (eclector.reader:check-feature-expression client feature-expression)
     t)
    (t
     (call-next-method))))

(test evaluate-feature-expression/customize
  "Test customizing the behavior of EVALUATE-FEATURE-EXPRESSION."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected) input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((eclector.reader:*client*
                               (make-instance 'feature-expression-client)))
                         (eclector.reader:read stream)))))
              (case expected
                (eclector.reader:single-feature-expected
                 (signals eclector.reader:single-feature-expected (do-it)))
                (type-error
                 (signals type-error (do-it)))
                (t
                 (is (eq expected (do-it))))))))
        '(;; Errors
          ("#+(not a b)                1 2" eclector.reader:single-feature-expected)
          ("#+(version-at-least)       1 2" type-error)
          ("#+(version-at-least 1)     1 2" type-error)
          ;; No errors
          ("#+common-lisp              1 2" 1)
          ("#+(not common-lisp)        1 2" 1)
          ("#+my-special-feature       1 2" 1)
          ("#+(and my-special-feature) 1 2" 1)
          ("#+(version-at-least \"1\") 1 2" 1))))
