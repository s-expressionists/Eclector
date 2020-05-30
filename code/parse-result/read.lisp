(cl:in-package #:eclector.parse-result)

;;; A list of sub-lists the form
;;;
;;;   (CHILDREN-OF-CURRENT-NODE CHILDREN-OF-PARENT ...)
;;;
(defvar *stack*)

(defvar *start*)

(defmethod eclector.reader:note-skipped-input
    ((client parse-result-client) input-stream reason)
  (let* ((start *start*)
         (end (source-position client input-stream))
         (range (make-source-range client start end))
         (parse-result (make-skipped-input-result
                        client input-stream reason range)))
    (when parse-result
      (push parse-result (second *stack*)))
    ;; Try to advance to the next non-whitespace input character,
    ;; then update *START*. This way, the source location for an
    ;; object subsequently read from INPUT-STREAM will not include
    ;; the whitespace.
    (eclector.reader::skip-whitespace* input-stream)
    (setf *start* (source-position client input-stream))))

;;; Establishing context

(defmethod eclector.reader:read-common :around
    ((client parse-result-client) input-stream eof-error-p eof-value)
  (let ((*stack* (list* '() *stack*)))
    (unless (eclector.reader::skip-whitespace* input-stream)
      (when eof-error-p
        (eclector.base:%reader-error input-stream 'eclector.reader:end-of-file))
      (return-from eclector.reader:read-common eof-value))
    (let* (;; *START* is used and potentially modified in
           ;; NOTE-SKIPPED-INPUT to reflect skipped input
           ;; (comments, reader macros, *READ-SUPPRESS*) before
           ;; actually reading something.
           (*start* (source-position client input-stream))
           (result (call-next-method))
           (children (reverse (first *stack*)))
           (end (source-position client input-stream))
           (source (make-source-range client *start* end))
           (parse-result (make-expression-result
                          client result children source)))
      (push parse-result (second *stack*))
      (values result parse-result))))

;;; Entry points

(defun read-aux (client input-stream eof-error-p eof-value preserve-whitespace-p)
  (multiple-value-bind (result parse-result orphan-results)
      (let ((eclector.reader:*client* client)
            (*stack* (list '())))
        (multiple-value-call #'values
          (eclector.reader::read-aux
           input-stream eof-error-p eof-value nil preserve-whitespace-p)
          (reverse (rest (first *stack*)))))
    ;; If we come here, that means that either the call to READ-AUX
    ;; succeeded without encountering end-of-file, or that
    ;; EOF-ERROR-P is false, end-of-file was encountered, and
    ;; EOF-VALUE was returned.  In the latter case, we want READ to
    ;; return EOF-VALUE.
    (values (if (and (null eof-error-p) (eq eof-value result))
                eof-value
                parse-result)
            orphan-results)))

(defun read (client &optional (input-stream *standard-input*)
                              (eof-error-p t)
                              (eof-value nil))
  (read-aux client input-stream eof-error-p eof-value nil))

(defun read-preserving-whitespace (client &optional
                                          (input-stream *standard-input*)
                                          (eof-error-p t)
                                          (eof-value nil))
  (read-aux client input-stream eof-error-p eof-value t))

(defun read-from-string (client string &optional
                                       (eof-error-p t)
                                       (eof-value nil)
                                       &key
                                       (start 0)
                                       (end nil)
                                       (preserve-whitespace nil))
  (let ((index))
    (multiple-value-bind (result orphan-results)
        (with-input-from-string (stream string :start start :end end
                                               :index index)
          (read-aux client stream eof-error-p eof-value preserve-whitespace))
      (values result index orphan-results))))
