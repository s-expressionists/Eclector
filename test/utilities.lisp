(cl:in-package #:eclector.test)

;;; Safe recursive equality

(defun equal* (left right
               &key (atom-test (lambda (recurse left right)
                                 (declare (ignore recurse))
                                 (if (consp right)
                                     nil
                                     (values (eql left right) t)))))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (left right)
               (let (old-right result resultp)
                 (cond ((multiple-value-bind (right foundp)
                            (gethash left seen)
                          (when foundp
                            (setf old-right right)
                            t))
                        (eq right old-right))
                       ((progn
                          (setf (values result resultp)
                                (funcall atom-test #'rec left right))
                          resultp)
                        result)
                       (t
                        (setf (gethash left seen) right)
                        (and (rec (car left) (car right))
                             (rec (cdr left) (cdr right))))))))
      (rec left right))))

(defun equalp* (left right)
  (flet ((atom-test (recurse left right)
           (typecase left
             (vector
              (values (and (typep right 'vector)
                           (equal (array-element-type left)
                                  (array-element-type right))
                           (= (length left) (length right))
                           (every recurse left right))
                      t))
             (atom
              (values (equalp left right) t))
             (t
              (values nil nil)))))
    (equal* left right :atom-test #'atom-test)))

(defun code-equal (left right)
  (flet ((compare (recurse left right)
           (cond ((and (symbolp right) (not (symbol-package right)))
                  (values (and (null (symbol-package left))
                               (string= (symbol-name left)
                                        (symbol-name right)))
                          t))
                 ((typep right '(cons (eql eclector.reader:quasiquote)))
                  (values t t))
                 ((pathnamep right)
                  (values (and (pathnamep left) (equalp left right)) t))
                 ((stringp right)
                  (values (and (stringp left) (string= left right)) t))
                 ((typep right '(and sequence (not cons)))
                  (values (and (eq (class-of left) (class-of right))
                               (= (length left) (length right))
                               (every recurse left right))
                          t))
                 ((typep right 'array) ; #0A() or higher rank
                  (values (equalp left right) t))
                 ((not (consp right))
                  (values (eql left right) t))
                 (t
                  (values nil nil)))))
    (equal* left right :atom-test #'compare)))

;;; Processing test cases

(defun wrap-in-expect (input-var form)
  `(macrolet ((expect (label test)
                `(is ,test
                     ,(format nil "~~@<For input ~~S, expected ~A ~
                                   ~~S but got ~~S~~@:>"
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

(defmacro do-stream-input-cases (((&optional (input-var (gensym "INPUT"))
                                             length-var)
                                  &rest lambda-list)
                                 form cases)
  (alexandria:with-unique-names (parameter)
    `(mapc (lambda (,parameter)
             (let* ((,input-var (format nil (first ,parameter)))
                    ,@(when length-var
                        `((,length-var (length ,input-var)))))
               (destructuring-bind (,@lambda-list) (rest ,parameter)
                 (macrolet ((with-stream ((stream-var) &body body)
                              `(with-input-from-string (,stream-var ,',input-var)
                                 (let ((values (multiple-value-list
                                                (progn ,@body))))
                                   (multiple-value-call #'values
                                     (values-list values)
                                     (file-position ,stream-var))))))
                   ,(wrap-in-expect input-var form)))))
           ,cases)))

;;; Checking expected errors

(defun check-signals-error
    (input expected-condition-type expected-position expected-length thunk)
  (handler-case
      (funcall thunk)
    (error (condition)
      ;; The caller must specify the exact expected condition type,
      ;; not some supertype.
      (is (type= expected-condition-type (type-of condition))
          "~@<When reading ~S, expected a condition of ~S, but got ~S~@:>"
          input expected-condition-type condition)
      ;; Check that STREAM-ERROR conditions contain a stream.
      (when (typep condition 'stream-error)
        (is (not (null (stream-error-stream condition)))
            "~@<When reading ~S and condition ~S was signaled, expected a ~
             non-null stream in the condition, but got ~S.~@:>"
            input condition nil)
        ;; Check effective stream position in CONDITION against
        ;; expected position.
        (unless (null expected-position)
          (let ((effective-position
                  (+ (eclector.base:stream-position condition)
                     (eclector.base:position-offset condition))))
            (is (= expected-position effective-position)
                "~@<When reading ~S and condition ~S was signaled, expected ~
                 position ~S, but got ~S.~@:>"
                input condition  expected-position effective-position)))
        (unless (null expected-length)
          (let ((length (eclector.base:range-length condition)))
            (is (= expected-length length)
                "~@<When reading ~S and condition ~S was signaled, expected ~
                 length ~S, but got ~S.~@:>"
                input condition expected-length length))))
      ;; Make sure CONDITION prints properly.
      (is (not (string= "" (princ-to-string condition)))
          "~@<When printing the signaled condition ~S expected a non-empty ~
           string, but got an empty string.~@:>"
          input condition))
    (:no-error (&rest values)
      (declare (ignore values))
      (fail "~@<When reading ~S, expected a condition of type ~S to be ~
             signaled but no condition was signaled.~@:>"
            input expected-condition-type))))

(defun %expected-condition-type (expected)
  (cond ((eq expected 'eclector.reader:feature-expression-type-error)
         'eclector.reader::feature-expression-type-error/reader)
        ((eq expected 'eclector.reader:single-feature-expected)
         'eclector.reader::single-feature-expected/reader)
        ((and (symbolp expected)
              (let ((package (symbol-package expected)))
                (and package (member (package-name package)
                                     '(#:eclector.base #:eclector.reader
                                       #:eclector.readtable)
                                     :test #'string=)))
              (subtypep expected 'condition))
         expected)))

(defun maybe-check-signals-error
    (input expected expected-position expected-length thunk)
  ;; If EXPECTED designates an Eclector condition type, ensure that a
  ;; condition of that type is signaled, otherwise decline so that
  ;; EXPECTED is interpreted as an expected normal return value.
  (if-let ((expected-condition-type (%expected-condition-type expected)))
    (progn
      (check-signals-error
       input expected-condition-type expected-position expected-length thunk)
      t)
    nil))

(defmacro error-case ((input expected-expression
                       &optional expected-position (expected-length 1))
                      &body clauses)
  (let* ((error-clause (find 'error clauses :key #'first))
         (error-body (rest error-clause))
         (other-clauses (remove 'error clauses :key #'first)))
    (once-only (expected-expression)
      ;; EXPECTED-EXPRESSION either designates an expected signaled
      ;; condition or an expected return value.
      `(or (maybe-check-signals-error
            ,input ,expected-expression ,expected-position ,expected-length
            (lambda () ,@error-body))
           (case ,expected-expression
             ,@other-clauses)))))
