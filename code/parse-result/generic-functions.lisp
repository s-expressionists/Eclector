(cl:in-package #:eclector.parse-result)

;;; Source location protocol

(defgeneric source-position (client stream)
  (:method (client stream)
    (declare (ignore client))
    (file-position stream)))

(defgeneric make-source-range (client start end)
  (:method (client start end)
    (declare (ignore client))
    (cons start end)))

;;; Parse result protocol

(defgeneric make-expression-result (client result children source))

(defgeneric make-skipped-input-result (client stream reason source)
  (:method (client stream reason source)
    (declare (ignore client stream reason source))))
