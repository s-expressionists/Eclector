(cl:in-package #:eclector.reader)

(defvar *labels*)

(defun read-aux
    (input-stream eof-error-p eof-value recursive-p preserve-whitespace-p)
  (let ((client *client*)
        (*preserve-whitespace* preserve-whitespace-p))
    (if recursive-p
        (read-common client input-stream eof-error-p eof-value)
        (let* ((*labels* (make-hash-table))
               (values (multiple-value-list
                        (read-common client input-stream eof-error-p eof-value)))
               (result (first values)))
          ;; *LABELS* maps labels to conses of the form
          ;;
          ;;   (TEMPORARY-OBJECT . FINAL-OBJECT)
          ;;
          ;; where TEMPORARY-OBJECT is EQ-comparable and its
          ;; sub-structure does not matter here. For the fixup step,
          ;; convert these conses into a hash-table mapping temporary
          ;; objects to final objects.
          (unless (zerop (hash-table-count *labels*))
            (let ((seen (make-hash-table :test #'eq))
                  (mapping (alexandria:alist-hash-table
                            (alexandria:hash-table-values *labels*)
                            :test #'eq)))
              (fixup client result seen mapping)))
          (values-list values)))))

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

(defun read-from-string (string &optional
                                (eof-error-p t)
                                (eof-value nil)
                                &key
                                (start 0)
                                (end nil)
                                (preserve-whitespace nil))
  (let ((index))
    (values (with-input-from-string (stream string :start start :end end
                                                   :index index)
              (read-aux stream eof-error-p eof-value nil preserve-whitespace))
            index)))

;;; Reading lists and READ-DELIMITED-LIST

(defun %invalid-end-of-list (stream char)
  (declare (ignore char))
  (%recoverable-reader-error stream 'invalid-context-for-right-parenthesis
                             :report 'ignore-trailing-right-paren))

(defun %handle-end-of-list (stream char)
  ;; If the call to SIGNAL returns, then there is no handler for this
  ;; condition, which means that the right parenthesis was found in a
  ;; context where it is not allowed.
  (signal-end-of-list char)
  (%invalid-end-of-list stream char))

(defun %maybe-end-of-list (stream char recursive-p)
  (unless (null char)
    (let ((next-char (read-char stream t nil recursive-p)))
      (if (char= next-char char)
          (%handle-end-of-list stream char)
          (unread-char next-char stream)))))

(defun %closing-delimiter (open-char close-char)
  ;; Not great, but we can't know the closing delimiter generally.
  (if (not (null close-char))
      close-char
      (case open-char
        (#\( #\))
        (t   open-char))))

(defun %read-delimited-list (input-stream open-char close-char recursive-p)
  (let ((reversed-result '())
        (tail nil)
        (*consing-dot-allowed-p* t))
    (block nil
      (handler-bind
          ((end-of-list
             (lambda (condition)
               (when (or (null close-char)
                         (char= close-char (%character condition)))
                 (return nil))))
           ((and end-of-file (not incomplete-construct))
             (lambda (condition)
               (%recoverable-reader-error
                input-stream 'unterminated-list
                :stream-position (stream-position condition)
                :delimiter (%closing-delimiter open-char close-char)
                :report 'use-partial-list)
               (return nil))))
        (loop for object = (progn
                             (%maybe-end-of-list input-stream close-char recursive-p)
                             (let ((*consing-dot-allowed-p* nil))
                               (read input-stream t nil recursive-p)))
              then (progn
                     (%maybe-end-of-list input-stream close-char recursive-p)
                     (read input-stream t nil recursive-p))
              if (eq object *consing-dot*)
              do (setf *consing-dot-allowed-p* nil)
                 (setf tail
                       (handler-case
                           (read input-stream t nil recursive-p)
                         (end-of-list (condition)
                           (%recoverable-reader-error
                            input-stream 'object-must-follow-consing-dot
                            :report 'inject-nil)
                           (unread-char (%character condition) input-stream)
                           nil)))
                 ;; This call to read must not return (it has to
                 ;; signal END-OF-LIST).
                 (read input-stream t nil t)
                 (%recoverable-reader-error
                  input-stream 'multiple-objects-following-consing-dot
                  :report 'ignore-object)
              else
              do (push object reversed-result))))
    (nreconc reversed-result tail)))

(defun read-delimited-list (char &optional (input-stream *standard-input*) recursive-p)
  (%read-delimited-list input-stream nil char recursive-p))
