(cl:in-package #:eclector.concrete-syntax-tree)

(defclass cst-client ()
  ())

(defvar *cst-client* (make-instance 'cst-client))

;;; A list of sub-lists the form
;;;
;;;   (CHILDREN-OF-CURRENT-NODE CHILDREN-OF-PARENT ...)
;;;
(defvar *stack*)

(defvar *start*)

(defun create-cst (client expression children source)
  (labels ((make-atom-cst (expression &optional source)
             (make-instance 'cst:atom-cst
                            :raw expression
                            :source source))
           (make-cons-cst (expression children &optional source)
             (destructuring-bind (car . cdr) expression
               (declare (ignore car))
               (destructuring-bind (car-children . cdr-children) children
                 (make-instance 'cst:cons-cst
                                :raw expression
                                :first car-children
                                :rest (if (atom cdr)
                                          (make-atom-cst cdr)
                                          (make-cons-cst cdr cdr-children))
                                :source source)))))
    (cond
      ((atom expression)
       (make-atom-cst expression source))
      ;; List structure with corresponding elements.
      ((and (eql (list-length expression) (length children))
            (every (lambda (sub-expression child)
                     (eql sub-expression (cst:raw child)))
                   expression children))
       (make-cons-cst expression children source))
      ;; Structure mismatch, try heuristic reconstruction.
      (t
       ;; We don't use
       ;;
       ;;   (cst:reconstruct expression children client)
       ;;
       ;; because we want SOURCE for the outer CONS-CST but not any of
       ;; its children.
       (destructuring-bind (car . cdr) expression
         (make-instance 'cst:cons-cst
                        :raw expression
                        :first (cst:reconstruct car children client)
                        :rest (cst:reconstruct cdr children client)
                        :source source))))))

(flet ((skip-whitespace (stream eof-error-p)
         (loop for char = (read-char stream nil nil)
               when (null char)
                 do (if eof-error-p
                        (error 'end-of-file :stream stream)
                        (return nil))
               while (eq (eclector.readtable:syntax-type
                          eclector.reader:*readtable* char)
                         :whitespace)
               finally (progn
                         (unread-char char stream)
                         (return t)))))

  (defmethod eclector.reader:note-skipped-input ((client cst-client) input-stream reason)
    (let* ((start *start*)
           (end (source-position input-stream client))
           (range (make-source-range client start end)))
      ;; Notify CLIENT of the skipped input and the reason.
      (record-skipped-input client input-stream reason range)
      ;; Try to advance to the next non-whitespace input character,
      ;; then update *START*. This way, the source location for an
      ;; object subsequently read from the stream will not include the
      ;; whitespace.
      (skip-whitespace input-stream nil) ; TODO
      (setf *start* (source-position input-stream client))))

  (defmethod eclector.reader:read-common :around ((client cst-client) input-stream eof-error-p eof-value)
    (if (boundp '*stack*)
        (let ((*stack* (cons '() *stack*)))
          (unless (skip-whitespace input-stream eof-error-p)
            (return-from eclector.reader:read-common eof-value))
          (let* (;; *START* is used and potentially modified in
                 ;; NOTE-SKIPPED-INPUT to reflect skipped input
                 ;; (comments, reader macros, *READ-SUPPRESS*) before
                 ;; actually reading something.
                 (*start* (source-position input-stream client))
                 (result (call-next-method))
                 (end (source-position input-stream client))
                 (source (make-source-range client *start* end))
                 ;; TODO reverse necessary?
                 (cst (create-cst client result (reverse (first *stack*)) source)))
            (push cst (second *stack*))
            result))
        (call-next-method))))

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
