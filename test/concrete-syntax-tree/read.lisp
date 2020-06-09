(cl:in-package #:eclector.concrete-syntax-tree.test)

(in-suite :eclector.concrete-syntax-tree)

(defmacro both ((operator cst))
  (let ((cst-operator (find-symbol (symbol-name operator)
                                   (find-package '#:cst))))
    (alexandria:once-only (cst)
      `(values (,cst-operator ,cst) (,operator (cst:raw ,cst))))))

;;; Smoke test

(defun is-consistent-with-raw (cst)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (cst)
               (if (gethash cst seen)
                   (return-from rec)
                   (setf (gethash cst seen) t))
               (multiple-value-bind (cst-atom-p raw-atom-p) (both (atom cst))
                 (is (eq raw-atom-p cst-atom-p))
                 (unless raw-atom-p
                   (multiple-value-bind (cst-car raw-car) (both (first cst))
                     (multiple-value-bind (cst-cdr raw-cdr) (both (rest cst))
                       (is (equal raw-car (cst:raw cst-car)))
                       (is (equal raw-cdr (cst:raw cst-cdr)))
                       (rec cst-car)
                       (rec cst-cdr)))))))
      (rec cst))))

(test read/smoke
  "Smoke test for the READ function."

  (do-stream-input-cases ((length) eof-error expected-raw
                          &optional expected-location)
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.concrete-syntax-tree:read stream eof-error :eof))))
      (error-case expected-raw
        (error (do-it))
        (:eof
         (is (eq :eof (do-it))))
        (t
         (multiple-value-bind (result orphan-results position) (do-it)
           (declare (ignore orphan-results))
           ;; CST result and its raw content.
           (is (typep result 'cst:cst))
           (is-consistent-with-raw result)
           (let ((raw (cst:raw result)))
             (expect "raw result" (equal expected-raw raw)))
           ;; Expected source location.
           (expect "source location" (equal expected-location (cst:source result)))
           ;; Consumed all input.
           (expect "position" (eql length position))))))
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

(test read-preserving-whitespace/smoke
  "Smoke test for the READ-PRESERVING-WHITESPACE function."

  (do-stream-input-cases (() eof-error-p eof-value
                          expected-result &optional expected-position)
      (flet ((do-it ()
               (with-stream (stream)
                 (eclector.concrete-syntax-tree:read-preserving-whitespace
                  stream eof-error-p eof-value))))
        (error-case expected-result
          (error (do-it))
          (:eof
           (multiple-value-bind (result orphan-results position) (do-it)
             (expect "result"         (eq :eof               result))
             (expect "orphan results" (eq '()                orphan-results))
             (expect "position"       (eql expected-position position))))
          (t
           (multiple-value-bind (result orphan-results position) (do-it)
             (is (typep result 'cst:cst))
             (let ((raw (cst:raw result)))
               (expect "raw results" (equal expected-result raw)))
             (expect "orphan results" (eq  '()               orphan-results))
             (expect "position"       (eql expected-position position))))))
    '((""        t   nil  eclector.reader:end-of-file)
      (""        nil :eof :eof                        0)

      (":foo"    t   nil  :foo                        4)
      (":foo "   t   nil  :foo                        4)
      (":foo  "  t   nil  :foo                        4)
      (":foo  1" t   nil  :foo                        4))))

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."

  (do-input-cases (input args expected-value &optional expected-position)
      (flet ((do-it ()
               (apply #'eclector.concrete-syntax-tree:read-from-string
                      input args)))
        (error-case expected-value
          (error (do-it))
          (:eof
           (multiple-value-bind (result position) (do-it)
             (expect "result"   (eq :eof               result))
             (expect "position" (eql expected-position position))))
          (t
           (multiple-value-bind (result position) (do-it)
             (is (typep result 'cst:cst))
             (let ((raw (cst:raw result)))
               (expect "raw result" (equal expected-value raw)))
             (expect "position" (eql expected-position position))))))
    '((""         ()                               eclector.reader:end-of-file)
      (""         (nil :eof)                       :eof                         0)

      (":foo 1 2" ()                               :foo                         5)

      ;; Start and end
      (":foo 1 2" (t nil :start 4)                 1                            7)
      (":foo 1 2" (t nil :end 3)                   :fo                          3)

      ;; Preserving whitespace
      (":foo 1"   (t nil :preserve-whitespace nil) :foo                         5)
      (":foo 1 "  (t nil :preserve-whitespace nil) :foo                         5)
      (":foo 1  " (t nil :preserve-whitespace nil) :foo                         5)
      (":foo 1 2" (t nil :preserve-whitespace nil) :foo                         5)

      (":foo 1"   (t nil :preserve-whitespace t)   :foo                         4)
      (":foo 1 "  (t nil :preserve-whitespace t)   :foo                         4)
      (":foo 1  " (t nil :preserve-whitespace t)   :foo                         4)
      (":foo 1 2" (t nil :preserve-whitespace t)   :foo                         4))))

;;; Source locations

(defun check-source-locations (cst expected-source-locations)
  (labels ((check (cst expected)
             (destructuring-bind (expected-location . children) expected
               (is (equal expected-location (cst:source cst)))
               (cond
                 ((not children)
                  (is-true (cst:atom cst)))
                 ((not (cst:consp cst))
                  (fail "Expected CONS CST, but got ~S" cst))
                 (t
                  (check (cst:first cst) (first children))
                  (check (cst:rest cst) (rest children)))))))
    (check cst expected-source-locations)))

(test read/source-locations
  "Test source locations assigned by READ."

  (do-stream-input-cases (() expected)
      (let ((result (with-stream (stream)
                      (eclector.concrete-syntax-tree:read stream))))
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
                                (scons (8 9)) ; 1
                                (scons ()
                                       (scons (10 11)) ; 2
                                       (scons ()))))

        ;; Nested reader macros
        ("#.(list* 1 '#.(list 2))" ,(scons (0 23)
                                           (scons (9 10)) ; 1
                                           (scons (12 22) ; #.(...)
                                                  (scons (20 21)) ; 2
                                                  (scons ()))))

        ;; Heuristic fails here
        ("#.(list 1 1)" ,(scons (0 12)
                                (scons (10 11)) ; second 1 (arbitrarily)
                                (scons ()
                                       (scons (10 11)) ; second 1 (arbitrarily)
                                       (scons ()))))))))

;;; Custom client

(defclass custom-client (eclector.concrete-syntax-tree:cst-client)
  ())

(defmethod eclector.parse-result:source-position
    ((client custom-client) (stream t))
  (- (call-next-method)))

(defmethod eclector.parse-result:make-source-range
    ((client custom-client) (start t) (end t))
  (vector start end))

(test read-cst/custom-client
  "Test using a custom client with READ."

  (let ((result (with-input-from-string (stream "#||# 1")
                  (let ((eclector.reader:*client* (make-instance 'custom-client)))
                    (eclector.concrete-syntax-tree:read stream)))))
    (is (equalp #(-5 -6) (cst:source result)))))

;;; Skipped input

(defclass skipped-input-recording-client
    (eclector.concrete-syntax-tree:cst-client)
  ((%skipped :accessor skipped
             :initform '())))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client skipped-input-recording-client) (stream t) (kind t) (source t))
  (alexandria:appendf (skipped client) (list (list kind source))))

(test make-skipped-input-result/smoke
  "Smoke test for the MAKE-SKIPPED-INPUT-RESULT function."

  (do-stream-input-cases ((length) expected)
    (flet ((do-it ()
             (let ((client
                     (make-instance 'skipped-input-recording-client)))
               (with-stream (stream)
                 (let ((eclector.reader:*client* client))
                   (eclector.concrete-syntax-tree:read stream))
                 (skipped client)))))
      (multiple-value-bind (skipped position) (do-it)
        (is (eql   length   position))
        (is (equal expected skipped))))
    '(;; No skipping
      ("1"            ())
      ;; Comments
      ("#||# 1"       ((:block-comment (0 . 4))))
      ("; test~% 1"   (((:line-comment . 1) (0 . 6))))
      (";; test~% 1"  (((:line-comment . 2) (0 . 7))))
      (";;; test~% 1" (((:line-comment . 3) (0 . 8))))
      ;; Reader conditionals
      ("#+(or) 1 2"   ((*read-suppress* (7 . 8))
                       ((:sharpsign-plus . (:or)) (0 . 9)))))))

;;; Regressions and other tests

(test make-expression-result/long-list
  "The method on MAKE-EXPRESSION-RESULT used to blow the stack for
   long lists."

  (let* ((length 200000)
         (input (format nil "(~{~A~^ ~})" (alexandria:iota length)))
         (result (with-input-from-string (stream input)
                   (eclector.concrete-syntax-tree:read stream)))
         (actual-length (loop for i from 0
                              for cst = result then (cst:rest cst)
                              while (cst:consp cst)
                              do (let ((raw (cst:raw (cst:first cst))))
                                   (unless (eql i raw)
                                     (fail "~@<Mismatch at index ~:D: ~
                                            ~S is not ~S to ~S~@:>"
                                           i raw 'eql i)))
                              count 1)))
    (is (= length actual-length))))
