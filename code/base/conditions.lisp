(cl:in-package #:eclector.base)

(defun %reader-error (stream datum &rest arguments)
  (let ((position (ignore-errors (file-position stream))))
    (apply #'error datum (append arguments
                                 (list :stream stream)
                                 (when position
                                   (list :stream-position position))))))

(define-condition stream-position-reader-error (acclimation:condition reader-error)
  ((%stream-position :initarg :stream-position
                     :reader stream-position)))
