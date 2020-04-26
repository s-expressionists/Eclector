(cl:in-package #:eclector.reader)

;;; Constituent traits
;;;
;;; Based on Table 2-8 in 2.1.4.2 Constituent Traits

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trait->index (trait)
    (ecase trait
      (:invalid                 #b00000001)
      (:alphabetic              #b00000010)
      (:alphadigit              #b00000100)
      ((:plus-sign :minus-sign) #b00001000)
      ((:dot :decimal-point)    #b00010000)
      (:package-marker          #b00100000)
      (:ratio-marker            #b01000000)
      ((:short-float-exponent-marker :single-float-exponent-marker
        :double-float-exponent-marker :long-float-exponent-marker
        :float-exponent-marker)
       #b10000000)))

  (declaim (type (simple-array (unsigned-byte 8) 1) +constituent-traits+))
  (defvar +constituent-traits+
    (let* ((raw '((#\Backspace . :invalid)
                  (#\Tab       . :invalid)
                  (#\Newline   . :invalid)    (#\+          . (:alphabetic :plus-sign))
                  (#\Linefeed  . :invalid)    (#\-          . (:alphabetic :minus-sign))
                  (#\Page      . :invalid)    (#\.          . (:alphabetic :dot :decimal-point))
                  (#\Return    . :invalid)    (#\/          . (:alphabetic :ratio-marker))
                  (#\Space     . :invalid)    (#\:          . :package-marker)
                  (#\Rubout    . :invalid)    ("0123456789" . :alphadigit)
                  (#\!         . :alphabetic) ("Aa"         . :alphadigit)
                  (#\"         . :alphabetic) ("Bb"         . :alphadigit)
                  (#\#         . :alphabetic) ("Cc"         . :alphadigit)
                  (#\$         . :alphabetic) ("Dd"         . (:alphadigit :double-float-exponent-marker))
                  (#\%         . :alphabetic) ("Ee"         . (:alphadigit :float-exponent-marker))
                  (#\&         . :alphabetic) ("Ff"         . (:alphadigit :single-float-exponent-marker))
                  (#\'         . :alphabetic) ("Gg"         . :alphadigit)
                  (#\(         . :alphabetic) ("Hh"         . :alphadigit)
                  (#\)         . :alphabetic) ("Ii"         . :alphadigit)
                  (#\*         . :alphabetic) ("Jj"         . :alphadigit)
                  (#\,         . :alphabetic) ("Kk"         . :alphadigit)
                  (#\;         . :alphabetic) ("Ll"         . (:alphadigit :long-float-exponent-marker))
                  (#\<         . :alphabetic) ("Mm"         . :alphadigit)
                  (#\=         . :alphabetic) ("Nn"         . :alphadigit)
                  (#\>         . :alphabetic) ("Oo"         . :alphadigit)
                  (#\?         . :alphabetic) ("Pp"         . :alphadigit)
                  (#\@         . :alphabetic) ("Qq"         . :alphadigit)
                  (#\[         . :alphabetic) ("Rr"         . :alphadigit)
                  (#\\         . :alphabetic) ("Ss"         . (:alphadigit :short-float-exponent-marker))
                  (#\]         . :alphabetic) ("Tt"         . :alphadigit)
                  (#\^         . :alphabetic) ("Uu"         . :alphadigit)
                  (#\_         . :alphabetic) ("Vv"         . :alphadigit)
                  (#\`         . :alphabetic) ("Ww"         . :alphadigit)
                  (#\|         . :alphabetic) ("Xx"         . :alphadigit)
                  (#\~         . :alphabetic) ("Yy"         . :alphadigit)
                  ("{}"        . :alphabetic) ("Zz"         . :alphadigit)))
           (table (make-array 0 :adjustable t)))
      (loop for (characters . traits) in raw
            do (loop for character in (typecase characters
                                        (character (list characters))
                                        (string (coerce characters 'list)))
                     for code = (char-code character)
                     for pattern = (reduce #'logior (alexandria:ensure-list traits)
                                           :key #'trait->index)
                     do (adjust-array table (max (length table) (1+ code))
                                      :initial-element 0)
                        (setf (aref table code) pattern)))
      (make-array (length table) :element-type '(unsigned-byte 8)
                                 :adjustable nil
                                 :initial-contents table))))

(macrolet ((define-trait-predicate (trait)
             (let ((name (alexandria:symbolicate '#:char- trait '#:-p)))
               `(progn
                  (declaim (inline ,name))
                  (defun ,name (char)
                    (let ((code (char-code char)))
                      (when (< code ,(length +constituent-traits+))
                        (logtest ,(trait->index trait) (aref +constituent-traits+ code)))))))))
  (define-trait-predicate :invalid)
  (define-trait-predicate :float-exponent-marker))

;;; Token interpretation

(defmethod interpret-symbol-token (client input-stream
                                   token
                                   position-package-marker-1
                                   position-package-marker-2)
  (let ((package-markers-end (or position-package-marker-2
                                 position-package-marker-1))
        (length (length token)))
    (when (and package-markers-end
               (> length 1)
               (= package-markers-end (1- length)))
      (%recoverable-reader-error
       input-stream 'symbol-name-must-not-end-with-package-marker
       :token token :report 'treat-as-escaped)
      (setf position-package-marker-1 nil
            position-package-marker-2 nil))
    (flet ((interpret (package symbol count)
             (interpret-symbol client input-stream package symbol count)))
      (cond ((null position-package-marker-1)
             (interpret :current token t))
            ((zerop position-package-marker-1)
             ;; We use PACKAGE-MARKERS-END so we can handle ::foo
             ;; which can happen when recovering from errors.
             (interpret :keyword
                        (subseq token (1+ package-markers-end))
                        t))
            ((not (null position-package-marker-2))
             (interpret (subseq token 0 position-package-marker-1)
                        (subseq token (1+ position-package-marker-2))
                        t))
            (t
             (interpret (subseq token 0 position-package-marker-1)
                        (subseq token (1+ position-package-marker-1))
                        nil))))))

(defmethod interpret-symbol (client input-stream
                             (package-indicator null) symbol-name internp)
  (make-symbol symbol-name))

(defmethod interpret-symbol (client input-stream
                             package-indicator symbol-name internp)
  (let ((package (case package-indicator
                   (:current *package*)
                   (:keyword (find-package "KEYWORD"))
                   (t        (or (find-package package-indicator)
                                 (%reader-error
                                  input-stream 'package-does-not-exist
                                  :package-name package-indicator))))))
    (if internp
        (intern symbol-name package)
        (multiple-value-bind (symbol status)
            (find-symbol symbol-name package)
          (cond ((null status)
                 (%reader-error input-stream 'symbol-does-not-exist
                                :package package
                                :symbol-name symbol-name))
                ((eq status :internal)
                 (%reader-error input-stream 'symbol-is-not-external
                                :package package
                                :symbol-name symbol-name))
                (t
                 symbol))))))

(declaim (inline reader-float-format))
(defun reader-float-format (&optional exponent-marker)
  (ecase exponent-marker
    ((nil #\e #\E)
     (case *read-default-float-format*
       (single-float 'single-float)
       (short-float 'short-float)
       (double-float 'double-float)
       (long-float 'long-float)
       (t
        ;; *read-default-float-format* may be some other type
        ;; *specifier which the implementation chooses to allow
        (if (subtypep *read-default-float-format* 'float)
            *read-default-float-format*
            (error 'invalid-default-float-format ; FIXME this is currently a READER-ERROR, but we do not have a stream at this point
                   :float-format *read-default-float-format*)))))
    ((#\f #\F) 'single-float)
    ((#\s #\S) 'short-float)
    ((#\d #\D) 'double-float)
    ((#\l #\L) 'long-float)))

(declaim (ftype (function ((integer 2 36)) function)
                make-integer-accumulator))
(defun make-integer-accumulator (base)
  (let ((value 0))
    (lambda (&optional char invalidatep)
      (cond ((null char)
             value)
            ((null value)
             nil)
            (t
             (let ((digit (digit-char-p char base)))
               (cond ((not (null digit))
                      (setf value (+ (* value base) digit)))
                     (invalidatep
                      (setf value nil))
                     (t
                      nil))))))))

(defmethod interpret-token (client input-stream token escape-ranges)
  (convert-according-to-readtable-case token escape-ranges)
  (let* ((length (length token))
         (remaining-escape-ranges escape-ranges)
         (escape-range (first remaining-escape-ranges))
         (sign 1)
         (decimal-mantissa (make-integer-accumulator 10))
         (numerator (make-integer-accumulator *read-base*))
         (denominator (make-integer-accumulator *read-base*))
         (fraction-numerator (make-integer-accumulator 10))
         (fraction-denominator 1)
         (exponent-sign 1)
         (exponent (make-integer-accumulator 10))
         (exponent-marker nil)
         (position-package-marker-1 nil)
         (position-package-marker-2 nil)
         (index -1))
    ;; The NEXT function and the NEXT-COND macro handle fetching the
    ;; next character and returning a symbol and going to tag SYMBOL
    ;; in case of an escape and as the default successor state.
    (flet ((next ()
             (incf index)
             (if (= length index)
                 nil
                 (values (aref token index)
                         (update-escape-ranges
                          index escape-range remaining-escape-ranges))))
           (symbol ()
             ;; This signals an error for ":" but accepts ":||". "::"
             ;; is handled via TWO-PACKAGE-MARKERS-MUST-NOT-BE-FIRST.
             (when (and (not escape-ranges)
                        (and (eql position-package-marker-1 0)
                             (= index 1)))
               (%reader-error
                input-stream 'symbol-name-must-not-be-only-package-markers
                :token token))
             (interpret-symbol-token
              client input-stream token
              position-package-marker-1
              position-package-marker-2))
           (return-float (&optional exponentp)
             (let ((magnitude (* (+ (funcall decimal-mantissa)
                                    (/ (funcall fraction-numerator)
                                       fraction-denominator))
                                 (if exponentp
                                     (expt 10 (* exponent-sign (funcall exponent)))
                                     1)))
                   (type (reader-float-format exponent-marker)))
               (return-from interpret-token
                 (* sign (coerce magnitude type))))))
      (macrolet ((next-cond ((char-var &optional
                                       return-symbol-if-eoi
                                       (colon-go-symbol t))
                             &body clauses)
                   (alexandria:with-unique-names (escapep-var)
                     `(multiple-value-bind (,char-var ,escapep-var) (next)
                        (cond
                          ,@(when return-symbol-if-eoi
                              `(((null ,char-var)
                                 (return-from interpret-token (symbol)))))
                          ((and ,char-var
                                (not ,escapep-var)
                                (char-invalid-p ,char-var))
                           (%recoverable-reader-error
                            input-stream 'invalid-constituent-character
                            :token (string ,char-var)
                            :report 'replace-invalid-character)
                           (setf (aref token index) #\_)
                           (go symbol))
                          (,escapep-var (go symbol))
                          ,@(when colon-go-symbol
                              `(((eql ,char-var #\:)
                                 (setf position-package-marker-1 index)
                                 (go symbol))))
                          ,@clauses
                          (t (go symbol)))))))
        (tagbody
         start
           ;; If we have a token of length 0, it must be a symbol in
           ;; the current package.
           (next-cond (char t)
             ((eql char #\+)
              (go sign))
             ((eql char #\-)
              (setf sign -1)
              (go sign))
             ((funcall decimal-mantissa char)
              (funcall numerator char t)
              (go decimal-integer))
             ((funcall numerator char)
              (go integer))
             ((eql char #\.)
              (go dot)))
         sign             ; We have a sign, i.e., #\+ or #\-.
           ;; If a sign is all we have, it is a symbol.
           (next-cond (char t)
             ((funcall decimal-mantissa char)
              (funcall numerator char t)
              (go decimal-integer))
             ((funcall numerator char)
              (go integer))
             ((eql char #\.)
              (go sign-dot)))
         dot
           (next-cond (char)
             ((not char)
              (cond ((not (null escape-ranges))
                     (return-from interpret-token (symbol)))
                    (*consing-dot-allowed-p*
                     (return-from interpret-token
                       *consing-dot*))
                    (t
                     (%reader-error input-stream 'invalid-context-for-consing-dot))))
             ((eql char #\.)
              (if (null escape-ranges)
                  (go maybe-too-many-dots)
                  (go symbol)))
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent)))
         maybe-too-many-dots
           ;; According to HyperSpec section 2.3.3 (The Consing Dot),
           ;; a token consisting solely of multiple dots (more than
           ;; one dot, no escapes) is illegal.
           (next-cond (char)
             ((not char)
              (%reader-error input-stream 'too-many-dots))
             ((eql char #\.)
              (go maybe-too-many-dots)))
         sign-dot                       ; sign decimal-point
           ;; If all we have is a sign followed by a dot, it must be a
           ;; symbol in the current package.
           (next-cond (char t)
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent)))
         decimal-integer                ; [sign] decimal-digit+
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (alexandria:if-let ((value (funcall numerator)))
                  (* sign value)
                  (symbol))))
             ((eql char #\.)
              (go decimal-integer-final))
             ((funcall decimal-mantissa char)
              (funcall numerator char t)
              (go decimal-integer))
             ((funcall numerator char)
              (go integer))
             ((eql char #\/)
              (go ratio-start))
             ((char-float-exponent-marker-p char)
              (setf exponent-marker char)
              (go float-exponent-start)))
         decimal-integer-final   ; [sign] decimal-digit+ decimal-point
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (* sign (funcall decimal-mantissa))))
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent))
             ((char-float-exponent-marker-p char)
              (setf exponent-marker char)
              (go float-exponent-start)))
         integer                 ; [sign] digit+
           ;; At least one digit is not decimal.
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (* sign (funcall numerator))))
             ((funcall numerator char)
              (go integer))
             ((eql char #\/)
              (go ratio-start)))
         ratio-start                    ; [sign] digit+ /
           (next-cond (char t)
             ((funcall denominator char)
              (go ratio)))
         ratio                          ; [sign] digit+ / digit+
           (next-cond (char)
             ((not char)
              (return-from interpret-token
                (alexandria:if-let ((numerator (funcall numerator)))
                  (let ((denominator (funcall denominator)))
                    (when (zerop denominator)
                      (%recoverable-reader-error
                       input-stream 'zero-denominator
                       :report 'replace-invalid-digit)
                      (setf denominator 1))
                    (* sign (/ numerator denominator)))
                  (symbol))))
             ((funcall denominator char)
              (go ratio)))
         float-no-exponent
           ;; [sign] decimal-digit* decimal-point decimal-digit+
           (next-cond (char)
             ((not char)
              (return-float))
             ((funcall fraction-numerator char)
              (setf fraction-denominator
                    (* fraction-denominator 10))
              (go float-no-exponent))
             ((char-float-exponent-marker-p char)
              (setf exponent-marker char)
              (go float-exponent-start)))
         float-exponent-start
           ;; [sign] decimal-digit+ exponent-marker
           ;; or
           ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker
           (next-cond (char t)
             ((eq char #\+)
              (go float-exponent-sign))
             ((eq char #\-)
              (setf exponent-sign -1)
              (go float-exponent-sign))
             ((funcall exponent char)
              (go float-exponent)))
         float-exponent-sign
           ;; [sign] decimal-digit+ exponent-marker sign
           ;; or
           ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker sign
           (next-cond (char t)
             ((funcall exponent char)
              (go float-exponent)))
         float-exponent
           ;; [sign] decimal-digit+ exponent-marker [sign] digit+
           ;; or
           ;; [sign] decimal-digit* decimal-point decimal-digit+
           ;; exponent-marker [sign] digit+
           (next-cond (char)
             ((not char)
              (return-float t))
             ((funcall exponent char)
              (go float-exponent)))
         symbol
           ;; a sequence of characters denoting a valid symbol name,
           ;; except that the last character might be a package
           ;; marker.
           (next-cond (char t nil)
             ((eq char #\:)
              (cond ((null position-package-marker-1)
                     (setf position-package-marker-1 index))
                    ((null position-package-marker-2)
                     (cond ((/= position-package-marker-1 (1- index))
                            (%recoverable-reader-error
                             input-stream 'two-package-markers-must-be-adjacent
                             :token token :report 'treat-as-escaped))
                           ((= position-package-marker-1 0)
                            (%recoverable-reader-error
                             input-stream 'two-package-markers-must-not-be-first
                             :token token :report 'treat-as-escaped)
                            (setf position-package-marker-2 index))
                           (t
                            (setf position-package-marker-2 index))))
                    (t
                     (%recoverable-reader-error
                      input-stream 'symbol-can-have-at-most-two-package-markers
                      :token token :report 'treat-as-escaped)))
              (go symbol))))))))
