(cl:in-package #:eclector.reader)

;;; Number literals

(define-kind number-kind (literal-kind) ())

;;; Rational literals

(define-kind rational-kind (number-kind) ())

(define-kind integer-kind (rational-kind) ())

(defmethod make-literal ((client t) (input-stream t) (kind integer-kind)
                         &key (sign 1) magnitude)
  (* sign magnitude))

(define-kind ratio-kind (rational-kind) ())

(defmethod make-literal ((client t) (input-stream t) (kind ratio-kind)
                         &key (sign 1) numerator (denominator nil denominator?))
  (cond ((not denominator?)
         (* sign numerator))
        ((zerop denominator)
         (%recoverable-reader-error
          input-stream 'zero-denominator
          :position-offset -1 :report 'replace-invalid-digit)
         (* sign numerator))
        (t
         (* sign (/ numerator denominator)))))

;;; Float literals

(define-kind float-kind (number-kind) ())

(define-kind explicit-format-float-kind (float-kind) ())

(define-kind default-float-kind (float-kind) ())

(define-kind concrete-float-kind (float-kind) ())

(defgeneric float-type (float-kind)) ; TODO: move to generic-functions.lisp

(macrolet ((define (kind-name float-type)
             `(progn
                (define-kind ,kind-name (concrete-float-kind) ())

                ;; TODO: ever used?
                (defmethod float-type ((float-kind ,kind-name))
                  ',float-type))))
  (define short-float-kind  short-float)
  (define single-float-kind single-float)
  (define double-float-kind double-float)
  (define long-float-kind   long-float))

;;; TODO: inline some of this?
(defun default-float-format (client)
  (let ((default-format (state-value client '*read-default-float-format*)))
    (case default-format
      (short-float  short-float-kind)
      (single-float single-float-kind)
      (double-float double-float-kind)
      (long-float   long-float-kind)
      ;; *READ-DEFAULT-FLOAT-FORMAT* may be some other type
      ;; specifier which the implementation chooses to allow.
      (t
       (if (subtypep default-format 'float) ; TODO maybe use (valid-state-value-p client '*read-default-float-format* default-format)? or is the protocol specified such that values returned by (state-value client '*read-default-float-format*) are by definition valid?
           default-format
           (values nil default-format))))))

(defun reader-float-format (client &optional exponent-marker)
  (ecase exponent-marker
    ((nil #\e #\E) (default-float-format client))
    ((#\s #\S)     short-float-kind)
    ((#\f #\F)     single-float-kind)
    ((#\d #\D)     double-float-kind)
    ((#\l #\L)     long-float-kind)
    ;; TODO: Do this as an extension
    ;; ((#\r #\R)     'rational-float-kind)
    ))

(defmethod make-literal
    ((client t) (input-stream t) (kind explicit-format-float-kind)
     &rest args &key exponent-marker exponent)
  (let ((kind (reader-float-format client exponent-marker)))
    (when (null kind)
      (multiple-value-bind (offset length)
          (if t ; exponentp
              (values
               (- (+ 1 (numeric-token-length exponent)))
               1)
              (values 0 0))           ; TODO: this case should be dead
        (%recoverable-reader-error
         input-stream 'invalid-default-float-format
         :position-offset offset :length length
         :exponent-marker exponent-marker
         :float-format :default-format
         :report 'use-replacement-float-format))
      (setf kind 'single-float-kind))
    (apply #'make-literal client input-stream kind args)))

(defmethod make-literal ((client t) (input-stream t) (kind default-float-kind)
                         &rest args &key)
  (let ((float-kind (default-float-format client)))
    (apply #'make-literal client input-stream float-kind args)))

(defun floating-point-overflow-in-float-literal
    (input-stream
     kind sign decimal-mantissa decimal-exponent exponent
     magnitude)
  (let ((type   (float-type kind))
        (length (+ (length (format nil "~D~@[E~D~]"
                                   decimal-mantissa exponent))
                   (if (zerop decimal-exponent) 0 1) ; TODO not accurate
                   )))
    (%recoverable-reader-error
     input-stream 'overflow-in-float
     :position-offset (- length)
     :operation       'coerce                 ; arithmetic-error
     :operands        (list magnitude type)
     :sign            sign                    ; overflow-in-float
     :mantissa        decimal-mantissa
     :exponent        exponent
     :float-format    type                    ; float-format-condition
     :report          'use-replacement-float-format ; TODO report
     )))

;;; Taken from SBCL code
;;; Truncate EXPONENT if it's too large for a float.
#+sbcl
(defun truncate-exponent (exponent mantissa)
  ;; Work with base-2 logarithms to avoid conversions to floats, and
  ;; convert to base-10 conservatively at the end.  Use the least
  ;; positive float, because denormalized exponent can be larger than
  ;; normalized.
  (let* ((max-exponent-bits (+ sb-vm:double-float-digits
                               sb-vm:double-float-bias))
         (mantissa-bits     (integer-length mantissa)))
    (if (minusp exponent)
        (max exponent (ceiling (- (+ max-exponent-bits mantissa-bits))
                               #.(cl:floor (cl:log 10 2))))
        (min exponent (floor (- max-exponent-bits mantissa-bits)
                             #.(cl:floor (cl:log 10 2)))))))

(flet ((magnitude (decimal-mantissa decimal-exponent
                   exponent? exponent-sign exponent)
         (let* ((exponent1 (- (if exponent?
                                  (* exponent-sign exponent)
                                  0)
                              decimal-exponent))
                ;; Truncate EXPONENT1 so that the result is roughly in
                ;; the right range but will still signal a
                ;; `floating-point-overflow-in-float-literal' if
                ;; appropriate.  The purpose of this truncation is to
                ;; defend against large intermediate bignums that
                ;; could exhaust the available memory.
                (exponent2 #+sbcl (truncate-exponent exponent1 decimal-mantissa)
                           #-sbcl exponent1))
           (* decimal-mantissa (expt 10 exponent2)))))

  (macrolet
      ((define (kind prototype)
         `(defmethod make-literal
              ((client t) (input-stream t) (kind ,kind)
               &key (sign 1)
                    (decimal-mantissa (alexandria:required-argument
                                       :decimal-mantissa))
                    (decimal-exponent (alexandria:required-argument
                                       :decimal-exponent))
                    (exponent-sign 1)
                    (exponent nil exponent?))
            (let ((magnitude (magnitude decimal-mantissa decimal-exponent
                                        exponent? exponent-sign exponent)))
              (handler-case
                  (* sign (float magnitude ,prototype))
                (floating-point-overflow ()
                  (floating-point-overflow-in-float-literal
                   input-stream kind sign decimal-mantissa decimal-exponent exponent magnitude)))))))
    (define short-float-kind  .0s0)
    (define single-float-kind .0f0)
    (define double-float-kind .0d0)
    (define long-float-kind   .0l0)))

;;; Complex literals

(define-kind complex-kind (number-kind) ())

(defmethod make-literal ((client t) (input-stream t) (kind complex-kind)
                         &key real-part imaginary-part)
  (complex real-part imaginary-part))

;;; Array literals

;;; String literals

(define-kind string-kind (literal-kind) ())

(defmethod make-literal ((client t) (input-stream t) (kind string-kind)
                         &key characters)
  ;; CHARACTERS is an adjustable array with a fill pointer. Make a
  ;; simple array.
  (copy-seq characters))

;;; Pathname literals

(define-kind pathname-kind (literal-kind) ())

;;; TODO: the entire method body is not reusable very well; clients
;;;       have to replace the method whole or nothing
(defmethod make-literal ((client t) (input-stream t) (kind pathname-kind)
                         &key namestring)
  (let ((namestring namestring))
    (unless (stringp namestring)
      (%recoverable-reader-error
       input-stream 'non-string-following-sharpsign-p
       :position-offset -1
       :expected-type   'string
       :datum           namestring
       :report          'replace-namestring)
      (setf namestring "."))
    (values (parse-namestring namestring))))

;;; Structure instance literals

(define-kind structure-instance-kind (literal-kind) ())

(defmethod make-literal
    ((client t) (input-stream t) (kind structure-instance-kind)
     &key type initargs)
  (locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:deprecation-condition))
           (make-structure-instance client type initargs)))
