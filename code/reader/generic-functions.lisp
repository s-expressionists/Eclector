(cl:in-package #:eclector.reader)

;;; Establishing context

(defgeneric call-as-top-level-read (client thunk input-stream
                                    eof-error-p eof-value preserve-whitespace-p))

(defgeneric read-common (client input-stream eof-error-p eof-value))

(defgeneric read-maybe-nothing (client input-stream eof-error-p eof-value))

(defgeneric note-skipped-input (client input-stream reason)
  (:method ((client t) (input-stream t) (reason t))
    (declare (ignore client input-stream reason))))

;;; Reader state protocol

(defgeneric valid-state-value-p (client aspect value))

(defgeneric state-value (client aspect))

(defgeneric call-with-state-value (client thunk aspect value))

;;; Default methods for the reader state protocol

(defmethod valid-state-value-p ((client t) (aspect (eql '*package*)) (value t))
  (typep value '(or package alexandria:string-designator)))

(defmethod state-value ((client t) (aspect (eql '*package*)))
  *package*)

(defmethod call-with-state-value ((client t)
                                  (thunk t)
                                  (aspect (eql '*package*))
                                  (value t))
  (locally
      #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (call-with-current-package client thunk value)))

(macrolet ((define (aspect &key (variable aspect) predicate)
             `(progn
                (defmethod valid-state-value-p ((client t)
                                                (aspect (eql ',aspect))
                                                (value t))
                  ,(if (null predicate)
                       't
                       `(,predicate value)))

                (defmethod state-value ((client t)
                                        (aspect (eql ',aspect)))
                  ,variable)

                (defmethod call-with-state-value ((client t)
                                                  (thunk t)
                                                  (aspect (eql ',aspect))
                                                  (value t))
                  (let ((,variable value))
                    (funcall thunk))))))
  (define cl:*readtable*              :variable eclector.reader:*readtable*
                                      :predicate eclector.readtable:readtablep)
  (define *read-suppress*)
  (define *read-eval*)
  (define *features*                  :predicate listp)
  (define *read-base*)
  (define *read-default-float-format*))

;;; Evaluate BODY in the context of CALL-WITH-STATE-VALUE calls for
;;; all aspect-value pairs in BINDINGS.
(defmacro with-state-values ((client &rest bindings) &body body)
  (alexandria:once-only (client)
    (labels ((rec (remainder)
               (destructuring-bind (&optional aspect value &rest rest)
                   remainder
                 (if (null aspect)
                     `(progn ,@body)
                     (alexandria:with-unique-names (thunk)
                       `(flet ((,thunk () ,(rec rest)))
                          (declare (dynamic-extent (function ,thunk)))
                          (call-with-state-value
                           ,client (function ,thunk) ,aspect ,value)))))))
      (rec bindings))))

;;; Reading tokens

(defgeneric read-token (client input-stream eof-error-p eof-value))

(defgeneric interpret-token (client input-stream token escape-ranges))

(defgeneric check-symbol-token (client input-stream
                                token escape-ranges
                                position-package-marker-1
                                position-package-marker-2))

(defgeneric interpret-symbol-token (client input-stream
                                    token
                                    position-package-marker-1
                                    position-package-marker-2))

(defgeneric interpret-symbol (client input-stream
                              package-indicator symbol-name internp))

;;; Kinds
;;;
;;; Used in the literal creation protocol and the expression creation
;;; protocol.

(defclass kind () ()) ; abstract
(defmethod print-object ((object kind) stream)
  (let ((name (class-name (class-of object))))
    (multiple-value-bind (variable-name defined?) (macroexpand name)
      (cond ((and defined? (boundp variable-name))
             (write name :stream stream))
            ((not *print-readably*)
             (print-unreadable-object (object stream :type t :identity t)))
            (t
             (error 'print-not-readable :object object))))))

(defmacro define-kind (name (&rest super-kind-names) (&rest slot-specifiers))
  (let* ((super-kinds   (or super-kind-names '(kind)))
         (variable-name (alexandria:symbolicate '#:* name '#:*)))
    `(progn
       (defclass ,name ,super-kinds ,slot-specifiers)
       (#-sbcl defvar #+sbcl sb-ext:defglobal ,variable-name nil)
       (when (null ,variable-name)
         (setf ,variable-name (make-instance ',name)))
       (define-symbol-macro ,name ,variable-name))))

;;; Literal creation protocol

(defclass literal-kind (kind) ()) ; abstract

(defgeneric make-literal (client class &key &allow-other-keys))

(define-kind character-kind (literal-kind) ())

(define-kind string-kind (literal-kind) ())
(defmethod make-literal ((client t) (kind string-kind) &key characters)
  ;; CHARACTERS is an adjustable array with a fill pointer. Make a
  ;; simple array.
  (copy-seq characters))

(define-kind number-kind (literal-kind) ())

(define-kind float-kind (number-kind) ())
(defmethod make-literal ((client t) (kind float-kind)
                         &key type sign decimal-mantissa
                              exponent-sign (exponent nil exponentp)
                              decimal-exponent)
  (let ((magnitude (* decimal-mantissa
                      (expt 10 (- (if exponentp
                                      (* exponent-sign exponent)
                                      0)
                                  decimal-exponent)))))
    (* sign (coerce magnitude type))))

;;; TODO separate file?
(define-kind rational-kind (number-kind) ())
(defmethod make-literal ((client t) (kind rational-kind)
                         &key sign numerator denominator)
  (* sign (if denominator
              (/ numerator denominator)
              numerator)))

(define-kind complex-kind (number-kind) ())
(defmethod make-literal ((client t) (kind complex-kind)
                         &key real-part imaginary-part)
  (complex real-part imaginary-part))

(define-kind structure-instance-kind (literal-kind) ())
(defmethod make-literal ((client t) (kind structure-instance-kind)
                         &key type initargs)
  (make-structure-instance client type initargs))

(define-kind pathname-kind (literal-kind) ())
(defmethod make-literal ((client t) (kind pathname-kind) &key namestring)
  (values (parse-namestring namestring)))

;;; Calling reader macros and behavior of standard reader macros

(defgeneric call-reader-macro (client input-stream char readtable)
  (:method ((client t) (input-stream t) (char t) (readtable t))
    (let ((function (eclector.readtable:get-macro-character readtable char)))
      (funcall function input-stream char))))

(defgeneric find-character (client designator)
  (:method ((client t) (designator character))
    designator)
  (:method ((client t) (designator string))
    (find-standard-character designator)))

(defgeneric make-structure-instance (client name initargs))

(defgeneric evaluate-expression (client expression)
  (:method ((client t) (expression t))
    (declare (ignore client))
    (eval expression)))

(defgeneric check-feature-expression (client feature-expression)
  (:method ((client t) (feature-expression t))
    (declare (ignore client))
    (check-standard-feature-expression feature-expression)))

(defgeneric evaluate-feature-expression (client feature-expression)
  (:method ((client t) (feature-expression t))
    (evaluate-standard-feature-expression
     client feature-expression
     :check (alexandria:curry #'check-feature-expression client)
     :recurse (alexandria:curry #'evaluate-feature-expression client))))

;;; Labeled objects and fixup

(defgeneric call-with-label-tracking (client thunk))

(defgeneric note-labeled-object (client input-stream label parent))

(defgeneric forget-labeled-object (client label))

(defgeneric find-labeled-object (client label))

(defgeneric make-labeled-object (client input-stream label parent))

(defgeneric labeled-object-state (client object)
  (:method (client object)
    (declare (ignore client object))
    ;; Default behavior: OBJECT is not a labeled object.
    nil))

(defgeneric finalize-labeled-object (client labeled-object object))

(defgeneric reference-labeled-object (client input-stream labeled-object))

(defgeneric fixup-graph-p (client root-labeled-object))

(defgeneric fixup-graph (client root-labeled-object &key object-key))

(defgeneric walk-fixup-tree (client function root-labeled-object))

(defgeneric fixup (client object seen-objects))

;;; Expression creation protocol

(defclass expression-kind (kind) ()) ; abstract

(defgeneric make-expression (client kind sub-expression))

(define-kind function-kind (expression-kind) ())
(defmethod make-expression ((client t) (kind function-kind) (sub-expression t))
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (wrap-in-function client sub-expression)))

(define-kind function-kind (expression-kind) ())
(defmethod make-expression ((client t) (kind function-kind) (sub-expression t))
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (wrap-in-function client sub-expression)))

(defclass any-quote-kind (expression-kind) ()) ; abstract

(define-kind quote-kind (any-quote-kind) ())
(defmethod make-expression ((client t) (kind quote-kind) (sub-expression t))
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (wrap-in-quote client sub-expression)))

(defclass any-quasiquote-kind (expression-kind) ()) ; abstract

(define-kind quasiquote-kind (any-quasiquote-kind any-quote-kind) ())
(defmethod make-expression ((client t) (kind quasiquote-kind) (sub-expression t))
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (wrap-in-quasiquote client sub-expression)))

(defclass any-unquote-kind (any-quasiquote-kind) ()) ; abstract

(define-kind unquote-kind (any-unquote-kind) ())
(defmethod make-expression ((client t) (kind unquote-kind) (sub-expression t))
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (wrap-in-unquote client sub-expression)))

(define-kind unquote-splicing-kind (any-unquote-kind) ())
(defmethod make-expression ((client t) (kind unquote-splicing-kind) (sub-expression t))
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
    (wrap-in-unquote-splicing client sub-expression)))
