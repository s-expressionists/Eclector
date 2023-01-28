(cl:in-package #:eclector.examples.highlight.cst)

;;; Concrete syntax tree protocol

(defgeneric source (node))

(defgeneric start (node))

(defgeneric end (node))

(defgeneric parent (node))

(defgeneric children (container))

(defgeneric find-child-starting-at (position container))

(defgeneric object (node))

;;; Default behavior

(defmethod parent ((node t))
  nil)

(defmethod children ((node t))
  '())

(defmethod find-child-starting-at ((position t) (container t))
  (find position (children container) :key #'start))

(defmethod object ((node t))
  nil)
