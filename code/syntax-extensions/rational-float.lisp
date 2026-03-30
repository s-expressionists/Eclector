(cl:defpackage #:eclector.syntax-extensions.rational-float
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria)

   (#:r #:eclector.reader))

  (:export
   #:client))

(cl:in-package #:eclector.syntax-extensions.rational-float)

(defclass client ()
  ())

;;; TODO: we would have to add `:float-exponent-marker' or
;;; `:custom-float-exponent-marker' or something similar to the
;;; constituent traits of #\r and #\R but that part of Eclector is not
;;; yet extensible.

;;; Change the `*read-default-float-format*' reader state aspect to
;;; allow the non-standard value `rational'.
;;;
;;; Unfortunately, we need a whole separate special variable because
;;; the host may restrict the permitted values of
;;; `cl:*read-default-float-format*' to the standard floating point
;;; types (and possibly implementation-specific extensions like SBCL
;;; permitting `rational' just like this extension).

;;; TODO: not ideal but i don't think there is a better way
(defvar *extended-read-default-float-format* 'single-float)

(defmethod r:valid-state-value-p
    ((client client) (aspect (eql '*read-default-float-format*)) (value t))
  ;; Accept `rational' as one additional value.
  (or (eq value 'rational) (call-next-method)))

(defmethod r:state-value
    ((client client) (aspect (eql '*read-default-float-format*)))
  *extended-read-default-float-format*)

(defmethod (setf r:state-value)
    ((new-value t) (client client) (aspect (eql '*read-default-float-format*)))
  (setf *extended-read-default-float-format* new-value))

(defmethod r:call-with-state-value ((client client)
                                    (thunk  t)
                                    (aspect (eql '*read-default-float-format*))
                                    (value  t))
  (let ((*extended-read-default-float-format* value))
    (funcall thunk)))

;;; Define an additional kind for the rational "floating point"
;;; literal and two methods which control the creation of such
;;; literals.

(r::define-kind rational-float-kind (r::concrete-float-kind)
  ())

;;; This method is called when a literal with a float exponent marker
;;; like 1e0 or 1R0 is encountered.  If the marker is #\r or #\R, call
;;; `make-rational' with `rational-float-kind'.  Otherwise, let the
;;; next method make a standard float literal.
(defmethod r:make-literal
    ((client client) (input-stream t) (kind r::explicit-format-float-kind)
     &rest args &key exponent-marker)
  (if (member exponent-marker '(#\r #\R))
      (apply #'r:make-literal client input-stream rational-float-kind args)
      (call-next-method)))

(defmethod r:make-literal ((client t) (input-stream t) (kind r::default-float-kind)
                           &rest args &key)
  ;; TODO: this is a bit bad because the next method will call
  ;; `state-value' again.
  (if (eq (r:state-value client '*read-default-float-format*) 'rational)
      (apply #'r:make-literal client input-stream rational-float-kind args)
      (call-next-method)))

(defmethod r:make-literal
    ((client client) (input-stream t) (kind rational-float-kind)
     &key (sign             1)
          (decimal-mantissa (a:required-argument :decimal-mantissa))
          (decimal-exponent (a:required-argument :decimal-exponent))
          (exponent-sign    1)
          (exponent         0))
  (declare (type (member -1 1)          sign exponent-sign)
           (type a:non-negative-integer decimal-mantissa decimal-exponent exponent))
  ;; All involved numbers are integers so the result is rational
  ;; (`integer' or `ratio').
  (* (* sign decimal-mantissa)
     (expt 10 (- (* exponent-sign exponent) decimal-exponent))))

;;; Test

(let ((eclector.base:*client* (make-instance 'client)))
  ;; Float exponent marker
  (assert (equal '(2469/200000 9)
                 (multiple-value-list
                  (r:read-from-string "12.345R-3"))))
  (assert (equal '(2469/200000 9)
                 (multiple-value-list
                  (r:read-from-string "12.345r-3"))))
  (assert (equal '(2469/200000 9)
                 (multiple-value-list
                  (let ((r:*readtable* (eclector.readtable:copy-readtable r:*readtable*)))
                    (setf (eclector.readtable:readtable-case r:*readtable*) :preserve)
                    (r:read-from-string "12.345r-3")))))
  ;; Default floating point format
  (assert (equal '(2469/200 6)
                 (multiple-value-list
                  (r:call-with-state-value
                   eclector.base:*client*
                   (lambda () (r:read-from-string "12.345"))
                   '*read-default-float-format*
                   'rational)))))
