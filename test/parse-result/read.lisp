(cl:in-package #:eclector.parse-result.test)

(in-suite :eclector.parse-result)

;;; The {PARSE,ATOM,CONS}-RESULT classes and the RESULTIFY function
;;; simulate what a client might do to represent parse results.

(defclass parse-result ()
  ((%raw    :initarg    :raw
            :reader     raw)
   (%source :initarg    :source
            :reader     source)
   (%kind   :allocation :class
            :reader     kind)))

(defmethod kind (thing)
  (declare (ignore thing))
  nil)

(defclass atom-result (parse-result)
  ((%kind :allocation :class
          :initform   'atom)))

(defclass cons-result (parse-result)
  ((%kind        :allocation :class
                 :initform   'cons)
   (%first-child :initarg    :first
                 :reader     first-child)
   (%rest-child  :initarg    :rest
                 :reader     rest-child)))

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

;;; A client that stores all result information in lists.

(defclass list-result-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client list-result-client) (result t) (children t) (source t))
  (list :result result :children children :source source))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client list-result-client) (stream t) (reason t) (source t))
  (list :reason reason :source source))

;;; Smoke test with parse results

(test read/smoke
  "Smoke test for the READ function."

  (do-stream-input-cases ((length) eof-error expected-raw
                          &optional expected-location
                                    (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.parse-result:read
                (make-instance 'simple-result-client) stream eof-error :eof))))
      (error-case expected-raw
        (error (do-it))
        (:eof
         (multiple-value-bind (result orphan-results position) (do-it)
           (expect "result"         (eq    expected-raw      result))
           (expect "orphan results" (equal '()               orphan-results))
           (expect "position"       (eql   expected-position position))))
        (t
         (multiple-value-bind (result orphan-results position) (do-it)
           (is (typep result 'parse-result))
           (expect "raw result"      (equal expected-raw      (raw result)))
           (expect "source location" (equal expected-location (source result)))
           (expect "orphan results"  (equal '()               orphan-results))
           (expect "position"        (eql   expected-position position))))))
    '(;; End of file
      (""              t   eclector.reader:end-of-file)
      (""              nil :eof)
      ("; comment"     t   eclector.reader:end-of-file)
      ("; comment"     nil :eof)
      ;; Actually reading something
      ("1"             t   1          ( 0 .  1))
      (" 1"            t   1          ( 1 .  2))
      ("1 "            t   1          ( 0 .  1))
      ("1 2"           t   1          ( 0 .  1) 2)
      ("(cons 1 2)"    t   (cons 1 2) ( 0 . 10))
      ("#+(or) `1 2"   t   2          (10 . 11))
      ("#|comment|# 1" t   1          (12 . 13))
      ("; comment~%1"  t   1          (10 . 11))
      ("(a . 2)"       t   (a . 2)    ( 0 .  7)))))

(test read-preserving-whitespace/smoke
  "Smoke test for the READ-PRESERVING-WHITESPACE function."

  (do-stream-input-cases (() eof-error-p eof-value expected-raw
                          &optional expected-location expected-position)
      (flet ((do-it ()
               (with-stream (stream)
                 (eclector.parse-result:read-preserving-whitespace
                  (make-instance 'simple-result-client)
                  stream eof-error-p eof-value))))
        (error-case expected-raw
          (error (do-it))
          (:eof
           (multiple-value-bind (result orphan-results position) (do-it)
             (expect "result"         (eq    expected-raw      result))
             (expect "orphan results" (equal '()               orphan-results))
             (expect "position"       (eql   expected-position position))))
          (t
           (multiple-value-bind (result orphan-results position) (do-it)
             (is (typep result 'parse-result))
             (expect "raw result"      (equal expected-raw      (raw result)))
             (expect "source location" (equal expected-location (source result)))
             (expect "orphan results"  (equal '()               orphan-results))
             (expect "position"        (eql   expected-position position))))))
    '((""        t   nil  eclector.reader:end-of-file)
      (""        nil :eof :eof nil     0)

      (":foo"    t   nil  :foo (0 . 4) 4)
      (":foo "   t   nil  :foo (0 . 4) 4)
      (":foo  "  t   nil  :foo (0 . 4) 4)
      (":foo  1" t   nil  :foo (0 . 4) 4))))

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."

  (do-input-cases ((input length) args expected-raw
                   &optional expected-location (expected-position length))
      (flet ((do-it ()
               (apply #'eclector.parse-result:read-from-string
                      (make-instance 'simple-result-client) input args)))
        (error-case expected-raw
          (error (do-it))
          (:eof
           (multiple-value-bind (result position orphan-results) (do-it)
             (expect "result"          (eq    expected-raw      result))
             (expect "orphan results"  (equal '()               orphan-results))
             (expect "position"        (eql   expected-position position))))
          (t
           (multiple-value-bind (result position orphan-results) (do-it)
             (expect "value"           (equal expected-raw      (raw result)))
             (expect "source location" (equal expected-location (source result)))
             (expect "orphan results"  (equal '()               orphan-results))
             (expect "position"        (eql   expected-position position))))))
    '((""         ()                               eclector.reader:end-of-file)
      (""         (nil :eof)                       :eof)

      (":foo 1 2" ()                               :foo (0 . 4) 5)

      ;; Start and end
      ;;
      ;; Implementations do not agree regarding what
      ;;
      ;;   (with-input-from-string (stream STRING :start START)
      ;;     (file-position stream))
      ;;
      ;; should return. So on some implementations, the source
      ;; position will be (5 . 6) instead of (1 . 2).
      (":foo 1 2" (t nil :start 4)                 1    (1 . 2) 7)
      (":foo 1 2" (t nil :end 3)                   :fo  (0 . 3) 3)

      ;; Preserving whitespace
      (":foo 1"   (t nil :preserve-whitespace nil) :foo (0 . 4) 5)
      (":foo 1  " (t nil :preserve-whitespace nil) :foo (0 . 4) 5)
      (":foo 1 2" (t nil :preserve-whitespace nil) :foo (0 . 4) 5)

      (":foo 1"   (t nil :preserve-whitespace t)   :foo (0 . 4) 4)
      (":foo 1  " (t nil :preserve-whitespace t)   :foo (0 . 4) 4)
      (":foo 1 2" (t nil :preserve-whitespace t)   :foo (0 . 4) 4))))

(test read-maybe-nothing/smoke
  "Smoke test for the READ-MAYBE-NOTHING function."

  (do-stream-input-cases ((length) (eof-error-p read-suppress)
                          expected-value &optional expected-kind
                                                   expected-parse-result
                                                   (expected-position length))
      (flet ((do-it ()
               (let ((client (make-instance 'list-result-client)))
                 (with-stream (stream)
                   (let (value kind parse-result)
                    (eclector.reader:call-as-top-level-read
                     client (lambda ()
                              (setf (values value kind parse-result)
                                    (let ((*read-suppress* read-suppress))
                                      (eclector.reader:read-maybe-nothing
                                       client stream eof-error-p :eof))))
                     stream eof-error-p :eof t)
                     (values value kind parse-result))))))
        (error-case expected-value
          (error (do-it))
          (t
           (multiple-value-bind (value kind parse-result position)
               (do-it)
             (expect "value"        (equal expected-value        value))
             (expect "kind"         (eq    expected-kind         kind))
             (expect "parse result" (equal expected-parse-result parse-result))
             (expect "position"     (eql   expected-position     position))))))
    '((""       (nil nil) :eof :eof)
      (""       (t   nil) eclector.reader:end-of-file)

      ("   "    (nil nil) nil :whitespace)
      ("   "    (nil nil) nil :whitespace)

      (";  "    (nil nil) nil :skip       (:reason (:line-comment . 1) :source (0 . 3))  )

      ("#||#"   (nil nil) nil :skip       (:reason :block-comment :source (0 . 4))       )
      ("#||# "  (nil nil) nil :skip       (:reason :block-comment :source (0 . 4))      4)
      ("#||#  " (nil nil) nil :skip       (:reason :block-comment :source (0 . 4))      4)
      ("#||#"   (nil t)   nil :skip       (:reason :block-comment :source (0 . 4))       )

      ("1"      (nil nil) 1   :object     (:result 1 :children () :source (0 . 1))       )
      ("1 "     (nil nil) 1   :object     (:result 1 :children () :source (0 . 1))      1)
      ("1"      (nil t)   nil :suppress   (:reason *read-suppress* :source (0 . 1))      )
      ("1 "     (nil t)   nil :suppress   (:reason *read-suppress* :source (0 . 1))     1))))

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

  (do-stream-input-cases (() expected)
    (let ((result (with-stream (stream)
                    (eclector.parse-result:read
                     (make-instance 'simple-result-client) stream))))
      (check-source-locations result expected))
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
                  (eclector.parse-result:read
                   (make-instance 'custom-source-position-client) stream))))
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

  (do-stream-input-cases ((length) read-suppress expected-result
                          &optional (expected-orphan-results '())
                                    (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.parse-result:read
                  (make-instance 'skipped-input-recording-client)
                  stream nil :eof)))))
      (multiple-value-bind (result orphan-results position) (do-it)
        (expect "result"         (equal expected-result         result))
        (expect "orphan results" (equal expected-orphan-results orphan-results))
        (expect "position"       (eql   expected-position       position))))
    '(;; Whitespace is not skipped input.
      ("1"                nil 1)
      (" 1"               nil 1)
      ("1 "               nil 1)
      ("1 2"              nil 1 () 2)

      ;; Toplevel comments
      ("#||# 1"           nil 1                          ((:block-comment (0 . 4))))
      ("#||# 1"           t   (*read-suppress* (5 . 6))  ((:block-comment (0 . 4))))
      ("; test"           nil :eof                       (((:line-comment . 1) (0 . 6))))
      ("; test~% 1"       nil 1                          (((:line-comment . 1) (0 . 7))))
      (";; test~% 1"      nil 1                          (((:line-comment . 2) (0 . 8))))
      (";;; test~% 1"     nil 1                          (((:line-comment . 3) (0 . 9))))
      ;; Toplevel reader conditionals
      ("#+(or) 1 2"       nil 2                          (((:sharpsign-plus . (:or)) (0 . 8))))
      ("#-(and) 1 2"      nil 2                          (((:sharpsign-minus . (:and)) (0 . 9))))

      ;; Non-toplevel comments
      ("(#||# 1)"         nil ((1) . ((:block-comment (1 . 5))
                                      1)))
      ("(~%; test~% 1)"   nil ((1) . (((:line-comment . 1) (2 . 9))
                                      1)))
      ("(~%;; test~% 1)"  nil ((1) . (((:line-comment . 2) (2 . 10))
                                      1)))
      ("(~%;;; test~% 1)" nil ((1) . (((:line-comment . 3) (2 . 11))
                                      1)))
      ;; Non-toplevel reader conditionals
      ("(#+(or) 1 2)"     nil ((2) . (((:sharpsign-plus . (:or)) (1 . 9)) 2)))

      ;; Order of skipped inputs
      ("#|1|# #|2|# 3"    nil 3                           ((:block-comment (0 . 5))
                                                           (:block-comment (6 . 11))))
      ("#|1|# #|2|# 3"    t   (*read-suppress* (12 . 13)) ((:block-comment (0 . 5))
                                                           (:block-comment (6 . 11))))

      ;; Non-toplevel suppressed objects
      ("(nil)"            t   (*read-suppress* (0 . 5))  ())
      ("#|1|# (nil)"      t   (*read-suppress* (6 . 11)) ((:block-comment (0 . 5)))))))
