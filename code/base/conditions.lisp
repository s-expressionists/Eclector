(cl:in-package #:eclector.base)

(defun %reader-error (stream datum
                      &rest arguments
                      &key
                      (stream-position (ignore-errors (file-position stream)))
                      &allow-other-keys)
  (apply #'error datum :stream stream :stream-position stream-position
         (alexandria:remove-from-plist arguments :stream-position)))

(defgeneric recovery-description (strategy language))

(defun %recoverable-reader-error (stream datum &rest arguments
                                               &key report &allow-other-keys)
  (restart-case
      (apply #'%reader-error stream datum
             (alexandria:remove-from-plist arguments :report))
    (recover ()
      :report (lambda (stream)
                (labels ((resolve (report)
                           (etypecase report
                             (symbol (let ((language (acclimation:language
                                                      acclimation:*locale*)))
                                       (resolve (recovery-description
                                                 report language))))
                             (string (format stream report))
                             (function (funcall report stream)))))
                  (resolve report)))
      (values))))

(defun recover (&optional condition)
  (alexandria:when-let ((restart (find-restart 'recover condition)))
    (invoke-restart restart)))

(define-condition stream-position-condition (condition)
  ((%stream-position :initarg :stream-position
                     :reader stream-position)))

(define-condition stream-position-reader-error (acclimation:condition
                                                stream-position-condition
                                                reader-error)
  ())

;;; Adds a stream position to CL:END-OF-FILE.
(define-condition end-of-file (acclimation:condition
                               stream-position-condition
                               cl:end-of-file)
  ())

(define-condition incomplete-construct (stream-position-reader-error)
  ())

(define-condition missing-delimiter (end-of-file incomplete-construct)
  ((%delimiter :initarg :delimiter :reader delimiter)))
