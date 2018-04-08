(cl:in-package #:eclector.concrete-syntax-tree)

(defgeneric source-position (stream client)
  (:method (stream client)
    (declare (ignore client))
    (file-position stream)))

(defgeneric make-source-range (client start end)
  (:method (client start end)
    (declare (ignore client))
    (cons start end)))

(defgeneric record-skipped-input (client stream reason source)
  (:method (client stream reason source)
    (declare (ignore client stream reason source))))
