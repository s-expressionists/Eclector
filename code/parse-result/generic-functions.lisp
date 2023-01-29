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
    (eclector.base:source-position nil stream)))

(defgeneric make-source-range (client start end)
  (:method (client start end)
    (eclector.base:make-source-range nil start end)))

;;; Parse result protocol

;;; The following two global variables are bound to marker objects
;;; that should be treated as opaque.  MAKE-EXPRESSION-RESULT is
;;; called with the value of **DEFINITION** as the second argument
;;; when the parse result being constructed corresponds to the
;;; definition of a labeled object.  Similarly, MAKE-EXPRESSION-RESULT
;;; is called with the value of **REFERENCE** as the second argument
;;; when the parse result being constructed corresponds to a reference
;;; to a labeled object.  For these calls, in slight abuse of the
;;; protocol, the third argument is the labeled object in question
;;; instead of a list of child parse results.  This convention allows
;;; clients to either construct a dedicated parse result that
;;; represents the definition/reference or return the parse result
;;; associated with the labeled object (that parse result corresponds
;;; to the object of the labeled object).
(#+sbcl sb-ext:defglobal #-sbcl defvar **definition** '#:%definition)
(#+sbcl sb-ext:defglobal #-sbcl defvar **reference** '#:%reference)

(defgeneric make-expression-result (client result children source)
  (:argument-precedence-order result client children source))

(defgeneric make-skipped-input-result (client stream reason source)
  (:method (client stream reason source)
    (declare (ignore client stream reason source))
    nil))
