(cl:in-package #:eclector.reader)

;;; Float literals

;;; These constants and the `truncate-exponent' function are based on
;;; SBCL code.
(defconstant +double-float-digits+ 53)
(defconstant +double-float-bias+   1022)

;;; Truncate EXPONENT if it is much larger than suitable for a float
;;; so that integer values in intermediate computations do not become
;;; unnecessarily (and dangerously) large.
(defun truncate-exponent (exponent mantissa)
  (declare (type integer exponent mantissa))
  ;; Work with base-2 logarithms to avoid conversions to floats, and
  ;; convert to base-10 conservatively at the end.  Use the least
  ;; positive float, because denormalized exponent can be larger than
  ;; normalized.
  (let* ((max-exponent-bits (+ +double-float-digits+
                               +double-float-bias+))
         (mantissa-bits (integer-length mantissa))
         (binary-to-decimal #.(cl:floor (cl:log 10 2))))
    (if (minusp exponent)
        (max exponent (ceiling (- (+ max-exponent-bits mantissa-bits))
                               binary-to-decimal))
        (min exponent (floor (- max-exponent-bits mantissa-bits)
                             binary-to-decimal)))))

(defun make-float-literal (stream &key type sign
                                       decimal-mantissa decimal-exponent
                                       exponent-sign (exponent nil exponentp))
  (declare (type (member nil -1 1) sign exponent-sign)
           (optimize speed))
  (let* ((effective-exponent (- (if exponentp
                                    (* (case exponent-sign
                                         ((nil) 1)
                                         (t exponent-sign))
                                       exponent)
                                    0)
                                decimal-exponent))
         ;; Truncate EFFECTIVE-EXPONENT so that, for bad (too big)
         ;; values of EFFECTIVE-EXPONENT, (expt 10 <exponent>) in the
         ;; following form does not needlessly produce huge
         ;; intermediate integer values and thereby consume dangerous
         ;; amounts of space and/or time.
         (truncated-exponent (truncate-exponent
                              effective-exponent decimal-mantissa))
         (magnitude (* decimal-mantissa (expt 10 truncated-exponent)))
         (effective-sign (case sign
                           ((nil) 1)
                           (t sign))))
    ;; The exponent is truncated enough to prevent problems with
    ;; intermediate computations but the following form can (and
    ;; should) still cause a floating point overflow if the result
    ;; cannot be represented.
    (handler-case
        (* effective-sign (coerce magnitude type))
      (floating-point-overflow ()
        (let ((length (+ (length (print (format nil "~D~:[~;E~:[~;+~]~2:*~D~]"
                                                decimal-mantissa exponent exponent-sign)))
                         (if (zerop decimal-exponent) 0 1) ; TODO not accurate
                         )))
          ;; The condition report might print the objects passed as
          ;; `:operands' which is fine since we pass truncated values.
          (%recoverable-reader-error
           stream 'overflow-in-float
           :position-offset (- length)  ; `stream-position-condition'
           :operation 'coerce           ; `arithmetic-error'
           :operands (list magnitude type) ; `arithmetic-error'
           :float-format type              ; `float-format-condition'
           :sign sign                      ; `overflow-in-float'
           :mantissa decimal-mantissa      ; `overflow-in-float'
           :exponent exponent              ; `overflow-in-float'
           :report 'use-replacement-float-format ; TODO report
           ))))))
