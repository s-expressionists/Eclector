(cl:in-package #:eclector.parse-result)

;;; A list of sub-lists the form
;;;
;;;   (CHILDREN-OF-CURRENT-NODE CHILDREN-OF-PARENT ...)
;;;
(defvar *stack*)

(defvar *start*)

(flet ((skip-whitespace (stream eof-error-p)
         (loop with readtable = eclector.reader:*readtable*
               for char = (read-char stream eof-error-p)
               when (null char)
                 do (return nil)
               while (eq (eclector.readtable:syntax-type readtable char)
                         :whitespace)
               finally (progn
                         (unread-char char stream)
                         (return t)))))

  (defmethod eclector.reader:note-skipped-input
      ((client parse-result-mixin) input-stream reason)
    (let* ((start *start*)
           (end (source-position client input-stream))
           (range (make-source-range client start end))
           (parse-result (make-skipped-input-result client input-stream reason range)))
      (when parse-result
        (push parse-result (second *stack*)))
      ;; Try to advance to the next non-whitespace input character,
      ;; then update *START*. This way, the source location for an
      ;; object subsequently read from the stream will not include the
      ;; whitespace.
      (skip-whitespace input-stream nil)
      (setf *start* (source-position client input-stream))))

  (defmethod eclector.reader:read-common :around
      ((client parse-result-mixin) input-stream eof-error-p eof-value)
    (if (boundp '*stack*)
        (let ((*stack* (cons '() *stack*)))
          (unless (skip-whitespace input-stream eof-error-p)
            (return-from eclector.reader:read-common eof-value))
          (let* (;; *START* is used and potentially modified in
                 ;; NOTE-SKIPPED-INPUT to reflect skipped input
                 ;; (comments, reader macros, *READ-SUPPRESS*) before
                 ;; actually reading something.
                 (*start* (source-position client input-stream))
                 (result (call-next-method))
                 (end (source-position client input-stream))
                 (source (make-source-range client *start* end))
                 (parse-result (make-expression-result client result (reverse (first *stack*)) source)))
            (push parse-result (second *stack*))
            (values result parse-result)))
        (call-next-method))))

(defun read (&rest arguments)
  (when (null eclector.reader:*client*)
    (error "~S must be bound to a client instance."
           'eclector.reader:*client*))

  (destructuring-bind (&optional eof-error-p eof-value) (rest arguments)
    (multiple-value-bind (result parse-result)
        (let ((*stack* (list '())))
          (apply #'eclector.reader:read arguments))
      ;; If we come here, that means that either the call to READ
      ;; succeeded without encountering end-of-file, or that
      ;; EOF-ERROR-P is false, end-of-file was encountered, and
      ;; EOF-VALUE was returned.  In the latter case, we want
      ;; READ to return EOF-VALUE.
      (if (and (null eof-error-p) (eq eof-value result))
          eof-value
          parse-result))))
