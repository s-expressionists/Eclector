(cl:in-package #:eclector)

(defvar *stack*)

(defun create-cst (expression children source)
  (if (atom expression)
      (make-instance 'cst:atom-cst
                     :raw expression
                     :source source)
      (labels ((aux (expression)
                 (let ((cst (find expression children :key #'cst:raw)))
                   (if (null cst)
                       (if (atom expression)
                           (cst:cst-from-expression expression)
                           (make-instance 'cst:cons-cst
                                          :raw expression
                                          :first (aux (car expression))
                                          :rest (aux (cdr expression))))
                       cst))))
        (make-instance 'cst:cons-cst
                       :raw expression
                       :first (aux (car expression))
                       :rest (aux (cdr expression))
                       :source source))))

(defgeneric source-position (stream client)
  (:method (stream client)
    (declare (ignore client))
    (file-position stream)))

(defmethod read-common :around (input-stream eof-error-p eof-value)
  (let ((*backquote-allowed-p* *backquote-in-subforms-allowed-p*)
        (*backquote-in-subforms-allowed-p* nil))
    (if (boundp '*stack*)
        (let ((*stack* (cons '() *stack*)))
          (loop for char = (read-char input-stream nil nil)
             when (null char)
             do (if eof-error-p
                    (error 'end-of-file :stream input-stream)
                    (return-from read-common eof-value))
             while (eq (sicl-readtable:syntax-type *readtable* char)
                       :whitespace)
             finally (unread-char  char input-stream))
          (let* ((start (source-position input-stream *client*))
                 (result (call-next-method))
                 (end (source-position input-stream *client*))
                 (source (cons start end))
                 (cst (create-cst result (reverse (first *stack*)) source)))
            (push cst (second *stack*))
            result))
        (call-next-method))))

(defun cst-read (&rest arguments)
  (destructuring-bind (&optional eof-error-p eof-value) (rest arguments)
    (let* ((*stack* (list '()))
           (result (apply #'read arguments)))
      ;; If we come here, that means that either the call to READ
      ;; succeeded without encountering end-of-file, or that
      ;; EOF-ERROR-P is false, end-of-file was encountered, and
      ;; EOF-VALUE was returned.  In the latter case, we want
      ;; CST-READ to return EOF-VALUE.
      (if (and (null eof-error-p) (eq eof-value result))
          eof-value
          (first (first *stack*))))))
