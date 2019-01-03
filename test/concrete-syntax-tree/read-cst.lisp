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

(test read-cst/smoke
  "Smoke test for the CST-READ function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind
              (input eof-error expected-raw &optional expected-location)
              input-and-expected
            (let ((input (format nil input)))
              (flet ((do-it ()
                       (with-input-from-string (stream input)
                         (values (eclector.concrete-syntax-tree:cst-read
                                  stream eof-error :eof)
                                 (file-position stream)))))
                (case expected-raw
                  (eclector.reader:end-of-file
                   (signals-printable eclector.reader:end-of-file (do-it)))
                  (:eof
                   (is (eq :eof (do-it))))
                  (t
                   (multiple-value-bind (result position) (do-it)
                     ;; CST result and its raw content.
                     (is (typep result 'cst:cst))
                     (is-consistent-with-raw result)
                     (let ((raw (cst:raw result)))
                       (is (equal expected-raw raw)))
                     ;; Expected source location.
                     (is (equal expected-location (cst:source result)))
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

(test read-cst/source-locations
  "Test source locations assigned by CST-READ."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected) input-expected
            (let ((result (with-input-from-string (stream input)
                            (eclector.concrete-syntax-tree:cst-read stream))))
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
  "Test using a custom client with CST-READ."

  (let ((result (with-input-from-string (stream "#||# 1")
                  (let ((eclector.reader:*client* (make-instance 'custom-client)))
                    (eclector.concrete-syntax-tree:cst-read stream)))))
    (is (equalp #(-5 -6) (cst:source result)))))

;;; Skipped input

(defclass skipped-input-recording-client
    (eclector.concrete-syntax-tree:cst-client)
  ((skipped :accessor skipped
            :initform '())))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client skipped-input-recording-client) (stream t) (kind t) (source t))
  (alexandria:appendf (skipped client) (list (list kind source))))

(test make-skipped-input-result/smoke
  "Smoke test for the MAKE-SKIPPED-INPUT-RESULT function."

  (mapc
   (lambda (input-expected)
     (destructuring-bind (input expected) input-expected
       (let ((input (format nil input)))
         (flet ((do-it ()
                  (let ((client
                          (make-instance 'skipped-input-recording-client)))
                    (with-input-from-string (stream input)
                      (values
                       (let ((eclector.reader:*client* client))
                         (eclector.concrete-syntax-tree:cst-read
                          stream))
                       (file-position stream)
                       (skipped client))))))
           (multiple-value-bind (result position skipped) (do-it)
             (declare (ignore result))
             (is (eql (length input) position))
             (is (equal expected skipped)))))))
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
