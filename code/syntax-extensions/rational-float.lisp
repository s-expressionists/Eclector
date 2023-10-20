(cl:defpackage #:eclector.syntax-extensions.rational-float
  (:use
   #:cl)

  (:export
   ))

(cl:in-package #:eclector.syntax-extensions.rational-float)

(defclass client ()
  ())

#+TODO (defmethod eclector.reader::reader-float-format (client &optional exponent-marker)
  (if (eql exponent-marker #\R)
      'rational
      (call-next-method)))

(defmethod eclector.reader:make-literal ((client client)
                                         (class  eclector.reader::float-kind)
                                         &key type sign decimal-mantissa decimal-exponent
                                              exponent-sign exponent)
  (case type
    (:custom (* (* sign decimal-mantissa)
                (expt 10 (- (* exponent-sign exponent) decimal-exponent))))
    (t       (call-next-method))))

; eclector.reader::trait->index

(let ((eclector.base:*client* (make-instance 'client)))
  (eclector.reader:read-from-string "1.234R2"))
(let ((eclector.base:*client* (make-instance 'client)))
  (eclector.reader:call-with-state-value
   eclector.base:*client*
   (eclector.reader:read-from-string "1R")
   '*read-default-float-format* :custom))
;;; 1.234R2 => 617/5 (= 123.4)
