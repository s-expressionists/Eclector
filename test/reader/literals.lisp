(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.literals
  :in :eclector.reader)

(defclass custom-literal-client () ())

(defmethod eclector.reader:make-literal
    ((client custom-literal-client) (input-stream t) (kind t)
     &rest args &key &allow-other-keys)
  (list* kind args))

(test custom-integer-literal
  (let ((client (make-instance 'custom-literal-client)))
    (do-stream-input-cases (() expected-literal)
      (is (equalp expected-literal (with-stream (stream)
                                     (let ((eclector.base:*client* client))
                                       (eclector.reader:read stream)))))

      `(("1"   (,eclector.reader::integer-kind          :magnitude 1))
        ("+2"  (,eclector.reader::integer-kind :sign  1 :magnitude 2))
        ("-3"  (,eclector.reader::integer-kind :sign -1 :magnitude 3))
        ("4."  (,eclector.reader::integer-kind          :magnitude 4))
        ("+5." (,eclector.reader::integer-kind :sign  1 :magnitude 5))
        ("-6." (,eclector.reader::integer-kind :sign -1 :magnitude 6))))))

(test custom-ratio-literal
  (let ((client (make-instance 'custom-literal-client)))
    (do-stream-input-cases (() expected-literal)
      (is (equalp expected-literal (with-stream (stream)
                                     (let ((eclector.base:*client* client))
                                       (eclector.reader:read stream)))))

      `(("1/2"  (,eclector.reader::ratio-kind          :numerator 1 :denominator 2))
        ("+3/4" (,eclector.reader::ratio-kind :sign  1 :numerator 3 :denominator 4))
        ("-5/6" (,eclector.reader::ratio-kind :sign -1 :numerator 5 :denominator 6))
        ;; Invalid
        ("1/0"  (,eclector.reader::ratio-kind          :numerator 1 :denominator 0))))))

(test custom-float-literal
  (let ((client (make-instance 'custom-literal-client)))
    (do-stream-input-cases (() expected-literal)
      (is (equalp expected-literal (with-stream (stream)
                                     (let ((eclector.base:*client* client))
                                       (eclector.reader:call-with-state-value client
                                        (lambda () (eclector.reader:read stream))
                                        '*read-default-float-format* 'single-float)))))

      `((".0"        (,eclector.reader::float-kind :type             single-float
                                                   :decimal-mantissa 0
                                                   :decimal-exponent 1))
        ("+.1"       (,eclector.reader::float-kind :type             single-float
                                                   :sign             1
                                                   :decimal-mantissa 1
                                                   :decimal-exponent 1))
        ("-.2"       (,eclector.reader::float-kind :type             single-float
                                                   :sign             -1
                                                   :decimal-mantissa 2
                                                   :decimal-exponent 1))
        ("3.4"       (,eclector.reader::float-kind :type             single-float
                                                   :decimal-mantissa 34
                                                   :decimal-exponent 1))
        ("+5.6"      (,eclector.reader::float-kind :type             single-float
                                                   :sign             1
                                                   :decimal-mantissa 56
                                                   :decimal-exponent 1))
        ("-7.8"      (,eclector.reader::float-kind :type             single-float
                                                   :sign             -1
                                                   :decimal-mantissa 78
                                                   :decimal-exponent 1))
        ;;
        (" 9.10e11"  (,eclector.reader::float-kind :type             single-float
                                                   :decimal-mantissa 910
                                                   :decimal-exponent 2
                                                   :exponent-sign    1
                                                   :exponent         11))
        ("+11.12e13" (,eclector.reader::float-kind :type             single-float
                                                   :sign             +1
                                                   :decimal-mantissa 1112
                                                   :decimal-exponent 2
                                                   :exponent-sign    1
                                                   :exponent         13))
        ("-14.15e16" (,eclector.reader::float-kind :type             single-float
                                                   :sign             -1
                                                   :decimal-mantissa 1415
                                                   :decimal-exponent 2
                                                   :exponent-sign    1
                                                   :exponent         16))
        ("17.18e-19" (,eclector.reader::float-kind :type             single-float
                                                   :decimal-mantissa 1718
                                                   :decimal-exponent 2
                                                   :exponent-sign    -1
                                                   :exponent         19))
        ("20.21e+22" (,eclector.reader::float-kind :type             single-float
                                                   :decimal-mantissa 2021
                                                   :decimal-exponent 2
                                                   :exponent-sign    +1
                                                   :exponent         22))))))
