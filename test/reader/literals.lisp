(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.literals
  :in :eclector.reader)

(defclass custom-literal-client () ())

(defmethod eclector.reader:make-literal
    ((client custom-literal-client) (input-stream t) (kind t)
     &rest args &key &allow-other-keys)
  (if (typep kind '(or eclector.reader::undecided-float-kind
                       eclector.reader::default-float-kind))
      (call-next-method) ; TODO: do we have to capture this somehow? (list :via-undecided (call-next-method))
      (list* kind args)))

(defun map-literal-test-cases (thunk cases)
  (do-stream-input-cases ((input) expected-literal)
    (let ((actual (with-stream (stream)
                    (let ((eclector.base:*client*
                            (make-instance 'custom-literal-client)))
                      (funcall thunk stream)))))
      (is (equalp expected-literal actual)
          "~@<For input ~S expected ~S but got ~S.~@:>"
          input expected-literal actual))
    cases))

(defmacro do-literal-test-cases ((stream-var) form cases)
  `(map-literal-test-cases (lambda (,stream-var) ,form) ,cases))

(test custom-integer-literals
  "Test construction of `integer' literals."
  (do-literal-test-cases (stream)
      (eclector.reader:read stream)
    `(("1"   (,eclector.reader::integer-kind          :magnitude 1))
      ("+2"  (,eclector.reader::integer-kind :sign  1 :magnitude 2))
      ("-3"  (,eclector.reader::integer-kind :sign -1 :magnitude 3))
      ("4."  (,eclector.reader::integer-kind          :magnitude 4))
      ("+5." (,eclector.reader::integer-kind :sign  1 :magnitude 5))
      ("-6." (,eclector.reader::integer-kind :sign -1 :magnitude 6)))))

(test custom-ratio-token-literals
  "Test construction of `ratio' literals via tokens."
  (do-literal-test-cases (stream)
      (eclector.reader:read stream)
    `(("1/2"  (,eclector.reader::ratio-kind          :numerator 1 :denominator 2))
      ("+3/4" (,eclector.reader::ratio-kind :sign  1 :numerator 3 :denominator 4))
      ("-5/6" (,eclector.reader::ratio-kind :sign -1 :numerator 5 :denominator 6))
      ;; Invalid
      ("1/0"  (,eclector.reader::ratio-kind          :numerator 1 :denominator 0)))))

(test custom-float-literal
  "Test construction of `float' literals."
  (do-literal-test-cases (stream)
      (eclector.reader:call-with-state-value
       client (lambda () (eclector.reader:read stream))
       '*read-default-float-format* 'single-float)
    `((".0"        (,eclector.reader::single-float-kind :decimal-mantissa 0
                                                        :decimal-exponent 1))
      ("+.1"       (,eclector.reader::single-float-kind :sign             1
                                                        :decimal-mantissa 1
                                                        :decimal-exponent 1))
      ("-.2"       (,eclector.reader::single-float-kind :sign             -1
                                                        :decimal-mantissa 2
                                                        :decimal-exponent 1))
      ("3.4"       (,eclector.reader::single-float-kind :decimal-mantissa 34
                                                        :decimal-exponent 1))
      ("+5.6"      (,eclector.reader::single-float-kind :sign             1
                                                        :decimal-mantissa 56
                                                        :decimal-exponent 1))
      ("-7.8"      (,eclector.reader::single-float-kind :sign             -1
                                                        :decimal-mantissa 78
                                                        :decimal-exponent 1))
      ;;
      (" 9.10e11"  (,eclector.reader::single-float-kind :exponent-marker  #\e
                                                        :exponent         11
                                                        :decimal-mantissa 910
                                                        :decimal-exponent 2))
      ("+11.12e13" (,eclector.reader::single-float-kind :exponent-marker  #\e
                                                        :exponent         13
                                                        :sign             +1
                                                        :decimal-mantissa 1112
                                                        :decimal-exponent 2))
      ("-14.15e16" (,eclector.reader::single-float-kind :exponent-marker  #\e
                                                        :exponent         16
                                                        :sign             -1
                                                        :decimal-mantissa 1415
                                                        :decimal-exponent 2))
      ("17.18e-19" (,eclector.reader::single-float-kind :exponent-sign    -1
                                                        :exponent-marker  #\e
                                                        :exponent         19
                                                        :decimal-mantissa 1718
                                                        :decimal-exponent 2))
      ("20.21e+22" (,eclector.reader::single-float-kind :exponent-sign    +1
                                                        :exponent-marker  #\e
                                                        :exponent         22
                                                        :decimal-mantissa 2021
                                                        :decimal-exponent 2)))))

(test custom-string-literals
  "Test construction of `string' literals."
  (do-literal-test-cases (stream)
      (eclector.reader:read stream)
    `(("\"foo\"" (,eclector.reader::string-kind :characters "foo")))))

;;; TODO: vector

;;; TODO: character

;;; TODO: ratio reader-macro

(test custom-ratio-reader-macro-literals
  "Test construction of `ratio' literals via reader macro."
  (do-literal-test-cases (stream)
    (eclector.reader:read stream)
    `(("#b-10" (,eclector.reader::ratio-kind :sign -1 :numerator 2))
      ("#b-10/11" (,eclector.reader::ratio-kind :sign -1 :denominator 3 :numerator 2)))))

(test custom-pathname-literals
  "Test construction of `pathname' literals."
  (do-literal-test-cases (stream)
    (eclector.reader:read stream)
    `(("#P\"foo\"" (,eclector.reader::pathname-kind
                    :namestring (,eclector.reader::string-kind
                                 :characters "foo"))))))
