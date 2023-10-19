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

;;; Creating s-expressions

(defgeneric wrap-in-function (client name)
  (:method (client name)
    (declare (ignore client))
    (list 'function name)))

(defgeneric wrap-in-quote (client material)
  (:method (client material)
    (declare (ignore client))
    (list 'quote material)))

(defgeneric wrap-in-quasiquote (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'quasiquote form)))

(defgeneric wrap-in-unquote (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'unquote form)))

(defgeneric wrap-in-unquote-splicing (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'unquote-splicing form)))
