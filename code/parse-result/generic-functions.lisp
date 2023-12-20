(cl:in-package #:eclector.parse-result)

;;; Source location protocol (has moved to base module)
;;;
;;; The default methods delegate to the default methods defined in the
;;; base module.
;;;
;;; This protocol will be removed from this module after a grace
;;; period.

(defgeneric source-position (client stream)
  (:method (client stream)
    (eclector.base:source-position client stream)))

(defgeneric make-source-range (client start end)
  (:method (client start end)
    (eclector.base:make-source-range client start end)))

;;; Parse result protocol

(defgeneric make-expression-result (client result children source)
  (:argument-precedence-order result client children source))

(defgeneric make-skipped-input-result (client stream reason source)
  (:method (client stream reason source)
    (declare (ignore client stream reason source))
    nil))

;;; The purpose of the following structure classes is to
;;; 1) indicate to the client that a parse result represents a labeled
;;;    object definition or labeled object reference
;;; 2) pass the labeled object to the client
;;; To this end, Eclector calls the MAKE-EXPRESSION-RESULT generic
;;; function with instances of the DEFINITION and REFERENCE classes as
;;; the RESULT argument.  Clients can specialize the RESULT parameter
;;; of methods on MAKE-EXPRESSION-RESULT to DEFINITION and REFERENCE
;;; and use the LABELED-OBJECT reader to obtain the labeled object
;;; from the RESULT argument.
(defstruct (labeled-object-result (:conc-name nil)
                                  (:constructor nil)
                                  (:predicate nil)
                                  (:copier nil))
  (labeled-object nil :read-only t))

(declaim (inline make-definition make-reference))
(defstruct (definition (:include labeled-object-result)
                       (:constructor make-definition (labeled-object))
                       (:predicate nil)
                       (:copier nil)))

(defstruct (reference (:include labeled-object-result)
                      (:constructor make-reference (labeled-object))
                      (:predicate nil)
                      (:copier nil)))
