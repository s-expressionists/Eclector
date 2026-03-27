(cl:defpackage #:eclector.syntax-extensions.rational-float
  (:use
   #:cl)

  (:export
   #:client))

(cl:in-package #:eclector.syntax-extensions.rational-float)

(defclass client ()
  ())

#+TODO (defmethod eclector.reader::reader-float-format (client &optional exponent-marker)
  (if (eql exponent-marker #\R)
      'rational
      (call-next-method)))

;;; TODO: not ideal but maybe there is no better way
(defvar *extended-read-default-float-format*)

(defmethod eclector.reader:valid-state-value-p
    ((client client) (aspect (eql '*read-default-float-format*)) (value t))
  (or (eq value :custom) (call-next-method)))

(defmethod eclector.reader:state-value
    ((client client) (aspect (eql '*read-default-float-format*)))
  *extended-read-default-float-format*)

(defmethod (setf eclector.reader:state-value)
    ((new-value t) (client client) (aspect (eql '*read-default-float-format*)))
  (setf *extended-read-default-float-format* new-value))

(defmethod eclector.reader:call-with-state-value
    ((client client)
     (thunk  t)
     (aspect (eql '*read-default-float-format*))
     (value  t))
  (let ((*extended-read-default-float-format* value))
    (funcall thunk)))

(defmethod eclector.reader:make-literal ((client       client)
                                         (input-stream t)
                                         (kind         eclector.reader::float-kind)
                                         &key type sign decimal-mantissa decimal-exponent
                                              exponent-sign exponent)
  (case type
    (:custom (* (* sign decimal-mantissa)
                (expt 10 (- (* exponent-sign exponent) decimal-exponent))))
    (t       (call-next-method))))

;;; TODO: eclector.reader::trait->index
