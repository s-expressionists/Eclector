(cl:in-package #:eclector.base)

(defun %reader-error (stream datum
                      &rest arguments
                      &key
                      (stream-position (ignore-errors (file-position stream)))
                      &allow-other-keys)
  (apply #'error datum :stream stream :stream-position stream-position
         (alexandria:remove-from-plist arguments :stream-position)))

(define-condition stream-position-reader-error (acclimation:condition reader-error)
  ((%stream-position :initarg :stream-position
                     :reader stream-position)))
