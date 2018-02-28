(cl:in-package #:eclector.concrete-syntax-tree)

(defclass cst-client ()
  ())

(defvar *cst-client* (make-instance 'cst-client))

(defgeneric source-position (stream client)
  (:method (stream client)
    (declare (ignore client))
    (file-position stream)))

;;; A list of sub-lists the form
;;;
;;;   (CHILDREN-OF-CURRENT-NODE CHILDREN-OF-PARENT ...)
;;;
(defvar *stack*)

(defun create-cst (expression children source)
  (if (atom expression)
      (make-instance 'cst:atom-cst
                     :raw expression
                     :source source)
      (labels ((cons-cst (expression &optional source)
                 (destructuring-bind (car . cdr) expression
                   (make-instance 'cst:cons-cst
                                  :raw expression
                                  :first (aux car)
                                  :rest (aux cdr)
                                  :source source)))
               (aux (expression)
                 (let ((cst (find expression children :key #'cst:raw)))
                   (cond
                     ((not (null cst))
                      cst)
                     ((atom expression)
                      (cst:cst-from-expression expression))
                     (t
                      (cons-cst expression))))))
        (cons-cst expression source))))

(defmethod eclector.reader:read-common :around ((client cst-client) input-stream eof-error-p eof-value)
  (if (boundp '*stack*)
      (let ((*stack* (cons '() *stack*)))
        (loop for char = (read-char input-stream nil nil)
           when (null char)
           do (if eof-error-p
                  (error 'end-of-file :stream input-stream)
                  (return-from eclector.reader:read-common eof-value))
           while (eq (eclector.readtable:syntax-type
                      eclector.reader:*readtable* char)
                     :whitespace)
           finally (unread-char char input-stream))
        (let* ((start (source-position input-stream client))
               (result (call-next-method))
               (end (source-position input-stream client))
               (source (cons start end))
               ;; TODO reverse necessary?
               (cst (create-cst result (reverse (first *stack*)) source)))
          (push cst (second *stack*))
          result))
      (call-next-method)))

(defun cst-read (&rest arguments)
  (destructuring-bind (&optional eof-error-p eof-value) (rest arguments)
    (let* ((eclector.reader:*client* (or eclector.reader:*client* *cst-client*))
           (*stack* (list '()))
           (result (apply #'eclector.reader:read arguments)))
      ;; If we come here, that means that either the call to READ
      ;; succeeded without encountering end-of-file, or that
      ;; EOF-ERROR-P is false, end-of-file was encountered, and
      ;; EOF-VALUE was returned.  In the latter case, we want
      ;; CST-READ to return EOF-VALUE.
      (if (and (null eof-error-p) (eq eof-value result))
          eof-value
          (first (first *stack*))))))
