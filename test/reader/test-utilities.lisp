(cl:in-package #:eclector.reader.test)

;;; Client for testing SHARPSIGN-S

(defclass sharpsign-s-client () ())

(defmethod eclector.reader:make-structure-instance
    ((client sharpsign-s-client) (name t) (initargs t))
  (list* name initargs))
