(cl:in-package #:eclector.reader)

(defvar *labels*)

(defun read-aux
    (input-stream eof-error-p eof-value recursive-p preserve-whitespace-p)
  (let ((*preserve-whitespace* preserve-whitespace-p))
    (if recursive-p
        (read-common *client* input-stream eof-error-p eof-value)
        (let* ((*labels* (make-hash-table))
               (result (read-common *client* input-stream eof-error-p eof-value)))
          ;; *labels* maps labels to conses of the form
          ;; (TEMPORARY-OBJECT . FINAL-OBJECT). For the fixup step,
          ;; these conses into a hash-table mapping temporary objects to
          ;; final objects.
          (unless (zerop (hash-table-count *labels*))
            (let ((seen (make-hash-table :test #'eq))
                  (mapping (alexandria:alist-hash-table
                            (alexandria:hash-table-values *labels*)
                            :test #'eq)))
              (fixup result seen mapping)))
          result))))

(defun read (&optional
               (input-stream *standard-input*)
               (eof-error-p t)
               (eof-value nil)
               (recursive-p nil))
  (read-aux input-stream eof-error-p eof-value recursive-p recursive-p))

(defun read-preserving-whitespace (&optional
                                     (input-stream *standard-input*)
                                     (eof-error-p t)
                                     (eof-value nil)
                                     (recursive-p nil))
  (read-aux input-stream eof-error-p eof-value recursive-p t))
