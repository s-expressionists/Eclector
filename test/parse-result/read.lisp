(cl:in-package #:eclector.parse-result.test)

(in-suite :eclector.parse-result)

;;; Test for *CLIENT* check

(test read/*client*
  "Test error signaled when calling READ with *CLIENT* NIL."

  (signals error
    (eclector.parse-result:read
     (make-string-input-stream "doesn't matter"))))

;;; The {PARSE,ATOM,CONS}-RESULT classes and the RESULTIFY function
;;; simulate what a client might do to represent parse results.

(defclass parse-result ()
  ((%raw
    :initarg :raw
    :reader raw)
   (%source
    :initarg :source
    :reader source)
   (%kind
    :allocation :class
    :reader kind)))

(defmethod kind (thing)
  nil)

(defclass atom-result (parse-result)
  ((%kind
    :allocation :class
    :initform 'atom)))

(defclass cons-result (parse-result)
  ((%kind
    :allocation :class
    :initform 'cons)
   (%first-child
    :initarg :first
    :reader first-child)
   (%rest-child
    :initarg :rest
    :reader rest-child)))

(defun resultify (raw results &optional source)
  (labels ((rec (raw-rest result-rest &optional source)
             (cond
               ((and (not (null (kind result-rest)))
                     (eq raw-rest (raw result-rest)))
                result-rest)
               ((atom raw-rest)
                (make-instance 'atom-result :raw raw-rest :source nil))
               (t
                (make-instance 'cons-result
                               :raw raw-rest
                               :source source
                               :first (rec (first raw-rest)
                                           (when (consp result-rest)
                                             (first result-rest)))
                               :rest (rec (rest raw-rest)
                                          (when (consp result-rest)
                                            (rest result-rest))))))))
    (rec raw results source)))

(defclass simple-result-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client simple-result-client) (result cons) (children t) (source t))
  (resultify result children source))

(defmethod eclector.parse-result:make-expression-result
    ((client simple-result-client) (result t) (children t) (source t))
  (make-instance 'atom-result :raw result :source source))

;;; Smoke test with parse results

(test read/smoke
  "Smoke test for the READ function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind
              (input eof-error expected-raw &optional expected-location)
              input-and-expected
            (let ((input (format nil input)))
              (flet ((do-it ()
                       (with-input-from-string (stream input)
                         (values (let ((eclector.reader:*client*
                                         (make-instance 'simple-result-client)))
                                   (eclector.parse-result:read
                                    stream eof-error :eof))
                                 (file-position stream)))))
                (case expected-raw
                  (eclector.reader:end-of-file
                   (signals eclector.reader:end-of-file (do-it)))
                  (:eof
                   (is (eq :eof (do-it))))
                  (t
                   (multiple-value-bind (result position) (do-it)
                     ;; PARSE-RESULT and its raw content.
                     (is (typep result 'parse-result))
                     ;; (is-consistent-with-raw result)
                     (let ((raw (raw result)))
                       (is (equal expected-raw raw)))
                     ;; Expected source location.
                     (is (equal expected-location (source result)))
                     ;; Consumed all input.
                     (is (eql (length input) position)))))))))

        '(;; End of file
          (""              t   eclector.reader:end-of-file)
          (""              nil :eof)
          ("; comment"     t   eclector.reader:end-of-file)
          ("; comment"     nil :eof)
          ;; Actually reading something
          ("(cons 1 2)"    t   (cons 1 2) ( 0 . 10))
          ("#+(or) `1 2"   t   2          (10 . 11))
          ("#|comment|# 1" t   1          (12 . 13))
          ("; comment~%1"  t   1          (10 . 11))
          ("(a . 2)"       t   (a . 2)    ( 0 .  7)))))

;;; Source locations

(defun check-source-locations (result expected-source-locations)
  (labels ((check (result expected)
             (destructuring-bind (expected-location . children) expected
               (is (equal expected-location (source result)))
               (cond
                 ((not children)
                  (is (eq 'atom (kind result))))
                 ((not (eq (kind result) 'cons))
                  (fail "Expected CONS-RESULT, but got ~S" result))
                 (t
                  (check (first-child result) (first children))
                  (check (rest-child result) (rest children)))))))
    (check result expected-source-locations))
  (is (not (null result))))

(test read/source-locations
  "Test source locations assigned by READ."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected) input-expected
            (let ((result (with-input-from-string (stream input)
                            (let ((eclector.reader:*client*
                                    (make-instance 'simple-result-client)))
                              (eclector.parse-result:read stream)))))
              (check-source-locations result expected))))
        (macrolet ((scons ((&optional start end) &optional car cdr)
                     `(cons ,(if start `(cons ,start ,end) 'nil)
                            ,(if car `(cons ,car ,cdr) 'nil))))
          `(;; Sanity check
            ("(1 2 3)"      ,(scons (0 7)
                                    (scons (1 2)) ; 1
                                    (scons ()
                                           (scons (3 4)) ; 2
                                           (scons ()
                                                  (scons (5 6)) ; 3
                                                  (scons ())))))

            ;; EQL children
            ("(1 1)"        ,(scons (0 5)
                                    (scons (1 2)) ; first 1
                                    (scons ()
                                           (scons (3 4)) ; second 1
                                           (scons ()))))

            ;; Simple reader macro
            ("#.(list 1 2)" ,(scons (0 12)
                                    (scons nil) ; 1
                                    (scons ()
                                           (scons nil) ; 2
                                           (scons ()))))

            ;; Nested reader macros
            ("#.(list* 1 '#.(list 2))" ,(scons (0 23)
                                               (scons nil) ; 1
                                               (scons nil ; #.(...)
                                                      (scons nil) ; 2
                                                      (scons ()))))

            ;; Heuristic fails here
            ("#.(list 1 1)" ,(scons (0 12)
                                    (scons nil) ; second 1 (arbitrarily)
                                    (scons ()
                                           (scons nil) ; second 1 (arbitrarily)
                                           (scons ()))))))))

;;; Custom source position

(defclass custom-source-position-client (simple-result-client)
  ())

(defmethod eclector.parse-result:source-position
    ((client custom-source-position-client) (stream t))
  (- (call-next-method)))

(defmethod eclector.parse-result:make-source-range
    ((client custom-source-position-client) (start t) (end t))
  (vector start end))

(test read/custom-source-position-client
  "Test using a custom client with READ."

  (let ((result (with-input-from-string (stream "#||# 1")
                  (let ((eclector.reader:*client*
                          (make-instance 'custom-source-position-client)))
                    (eclector.parse-result:read stream)))))
    (is (equalp #(-5 -6) (source result)))))

;;; Skipped input

(defclass skipped-input-recording-client
    (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client skipped-input-recording-client) (result t) (children t) (source t))
  (if (null children)
      result
      (cons result children)))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client skipped-input-recording-client) (stream t) (kind t) (source t))
  (list kind source))

(test make-skipped-input-result/smoke
  "Smoke test for the MAKE-SKIPPED-INPUT-RESULT function."

  (mapc
   (lambda (input-expected)
     (destructuring-bind (input expected-result
                          &optional expected-orphan-results)
         input-expected
       (let ((input (format nil input)))
         (flet ((do-it ()
                  (let ((client
                          (make-instance 'skipped-input-recording-client)))
                    (with-input-from-string (stream input)
                      (multiple-value-call #'values
                       (let ((eclector.reader:*client* client))
                         (eclector.parse-result:read stream))
                       (file-position stream))))))
           (multiple-value-bind (result orphan-results position) (do-it)
             (is (equal expected-result result))
             (is (equal expected-orphan-results orphan-results))
             (is (eql (length input) position)))))))
   '(;; No skipping
     ("1"                1 ())

     ;; Toplevel Comments
     ("#||# 1"           1 ((:block-comment (0 . 4))))
     ("; test~% 1"       1 (((:line-comment . 1) (0 . 6))))
     (";; test~% 1"      1 (((:line-comment . 2) (0 . 7))))
     (";;; test~% 1"     1 (((:line-comment . 3) (0 . 8))))
     ;; Toplevel Reader conditionals
     ("#+(or) 1 2"       (2 . (((:or) . (:or))
                               (*read-suppress* (7 . 8))
                               nil))
                         ((:reader-macro (0 . 9))))

     ;; Non-toplevel Comments
     ("(#||# 1)"         ((1) . ((:block-comment (1 . 5))
                                 1)))
     ("(~%; test~% 1)"   ((1) . (((:line-comment . 1) (2 . 8))
                                 1)))
     ("(~%;; test~% 1)"  ((1) . (((:line-comment . 2) (2 . 9))
                                 1)))
     ("(~%;;; test~% 1)" ((1) . (((:line-comment . 3) (2 . 10))
                                 1)))
     ;; Non-toplevel Reader conditionals
     ("(#+(or) 1 2)"     ((2) . ((:reader-macro (1 . 10))
                                 (2 . (((:or) . (:or))
                                       (*read-suppress* (8 . 9))
                                       nil))))))))
