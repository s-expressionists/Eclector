(cl:in-package #:eclector.concrete-syntax-tree)

;;; Fixup

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object cst:atom-cst)
                                  seen-objects)
  (declare (ignore seen-objects)))

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object cst:cons-cst)
                                  seen-objects)
  (macrolet ((fixup-place (place)
               ;; Determine the labeled object state of the raw value
               ;; of the CST stored in PLACE. For a finalized labeled
               ;; object, create a "reference" CST object (which may
               ;; just be the parse result for the circular object,
               ;; depending on the client) and replace the value of
               ;; PLACE with that reference CST object.
               `(let* ((current-value ,place)
                       (current-raw-value (cst:raw current-value)))
                  (eclector.reader:fixup-case (client current-raw-value)
                    (() ; not a labeled object
                     (eclector.reader:fixup client current-value seen-objects))
                    (() ; finalized labeled object
                     (let ((source (cst:source current-value)))
                       ;; CURRENT-RAW-VALUE is a finalized labeled
                       ;; object from which the (circular) final raw
                       ;; value and parse result can be obtained.
                       (let ((reference (eclector.parse-result:make-reference
                                         current-raw-value)))
                         (declare (dynamic-extent reference))
                         (setf ,place (eclector.parse-result:make-expression-result
                                       client reference '() source)))))))))
    (fixup-place (slot-value object 'cst::%first))
    (fixup-place (slot-value object 'cst::%rest))))

;;; Explicit definition and reference CSTs
;;;
;;; These mixins allow clients to wrap CSTs for defined and referenced
;;; labeled objects in DEFINITION-CST and REFERENCE-CST instances
;;; respectively.

(defclass wrapper-cst (cst:cst)
  ((%target :initarg :target
            :reader  target)))

(defclass definition-cst (wrapper-cst) ())

(defclass reference-cst (wrapper-cst) ())

(defclass definition-csts-mixin () ())

(defclass reference-csts-mixin () ())

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object definition-cst)
                                  seen-objects)
  (eclector.reader:fixup client (target object) seen-objects))

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object reference-cst)
                                  seen-objects)
  (declare (ignore seen-objects))) ; nothing to do

(macrolet ((labeled-object-result (client result source class)
             `(let ((labeled-object (eclector.parse-result:labeled-object ,result)))
                (multiple-value-bind (state object labeled-object parse-result)
                    (eclector.reader:labeled-object-state ,client labeled-object)
                  (declare (ignore labeled-object state))
                  (make-instance ',class :source ,source
                                         :raw object
                                         :target parse-result)))))

  (defmethod eclector.parse-result:make-expression-result
      ((client definition-csts-mixin)
       (result eclector.parse-result:definition)
       children
       source)
    (declare (ignore children))
    (labeled-object-result client result source definition-cst))

  (defmethod eclector.parse-result:make-expression-result
      ((client reference-csts-mixin)
       (result eclector.parse-result:reference)
       children
       source)
    (declare (ignore children))
    (labeled-object-result client result source reference-cst)))
