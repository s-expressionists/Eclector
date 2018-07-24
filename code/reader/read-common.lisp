(cl:in-package #:eclector.reader)

(defun read-char (stream &optional eof-error-p eof-value recursive-p)
  (if eof-error-p
      (let ((result (cl:read-char stream nil '#1=#.(gensym "EOF") recursive-p)))
        (if (eq result '#1#)
            (%reader-error stream 'end-of-file)
            result))
      (cl:read-char stream nil eof-value recursive-p)))

(define-compiler-macro read-char
    (&whole whole stream &optional eof-error-p eof-value recursive-p)
  (if (and (constantp eof-error-p) (not (eval eof-error-p)))
      `(cl:read-char ,stream nil ,eof-value ,recursive-p)
      whole))

(defmethod call-reader-macro (client input-stream char readtable)
  (let ((function (eclector.readtable:get-macro-character readtable char)))
    (funcall function input-stream char)))

(defmethod read-common :around (client input-stream eof-error-p eof-value)
  (let ((*input-stream* input-stream)
        (*backquote-allowed-p* *backquote-in-subforms-allowed-p*)
        (*backquote-in-subforms-allowed-p* nil))
    (call-next-method)))

(defmethod read-common (client input-stream eof-error-p eof-value)
  (tagbody
   step-1-start
     (let ((*skip-reason* nil)
           (char (read-char input-stream eof-error-p)))
       (when (null char)
         (return-from read-common eof-value))
       (case (eclector.readtable:syntax-type *readtable* char)
         (:whitespace
          (go step-1-start))
         ((:terminating-macro :non-terminating-macro)
          (let ((values (multiple-value-list
                         (call-reader-macro
                          client input-stream char *readtable*))))
            (cond
              ((null values)
               (note-skipped-input client input-stream
                                   (or *skip-reason* :reader-macro))
               (go step-1-start))
              ;; This case takes care of reader macro not returning
              ;; nil when *READ-SUPPRESS* is true.
              (*read-suppress*
               (note-skipped-input client input-stream
                                   (or *skip-reason* '*read-suppress*))
               (return-from read-common nil))
              (t
               (return-from read-common (car values))))))
         (t
          (unread-char char input-stream)
          (return-from read-common
            (read-token client input-stream eof-error-p eof-value)))))))

(defmethod read-token (client input-stream eof-error-p eof-value)
  (let ((token (make-array 100
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0))
        (escape-ranges '()))
    (flet ((push-char (char)
             (vector-push-extend char token)
             char)
           (start-escape ()
             (push (cons (length token) nil) escape-ranges))
           (end-escape ()
             (setf (cdr (first escape-ranges)) (length token)))
           (read-char-handling-eof ()
             (let ((char (read-char input-stream eof-error-p)))
               (if (not (null char))
                   char
                   (return-from read-token eof-value)))))
      (tagbody
         ;; This function is only called when a character is available
         ;; in INPUT-STREAM.
         (let ((char (read-char input-stream)))
           (ecase (eclector.readtable:syntax-type *readtable* char)
             (:single-escape
              (start-escape)
              (push-char (read-char-handling-eof))
              (end-escape)
              (go step-8-even-escapes))
             (:multiple-escape
              (start-escape)
              (go step-9-odd-escapes))
             (:constituent
              (push-char char)
              (go step-8-even-escapes))))
       step-8-even-escapes
         (let ((char (read-char input-stream nil nil)))
           (when (null char)
             (go step-10-terminate-token))
           (ecase (eclector.readtable:syntax-type *readtable* char)
             ((:constituent :non-terminating-macro)
              (push-char char)
              (go step-8-even-escapes))
             (:single-escape
              (start-escape)
              (push-char (read-char-handling-eof))
              (end-escape)
              (go step-8-even-escapes))
             (:multiple-escape
              (start-escape)
              (go step-9-odd-escapes))
             (:terminating-macro
              (unread-char char input-stream)
              (go step-10-terminate-token))
             (:whitespace
              (when *preserve-whitespace*
                (unread-char char input-stream))
              (go step-10-terminate-token))))
       step-9-odd-escapes
         (let ((char (read-char-handling-eof)))
           (ecase (eclector.readtable:syntax-type *readtable* char)
             ((:constituent :terminating-macro
               :non-terminating-macro :whitespace)
              (push-char char)
              (go step-9-odd-escapes))
             (:single-escape
              (push-char (read-char-handling-eof))
              (go step-9-odd-escapes))
             (:multiple-escape
              (end-escape)
              (go step-8-even-escapes))))
       step-10-terminate-token
         (return-from read-token
           (cond
             (*read-suppress*
              (note-skipped-input client input-stream
                                  (or *skip-reason* '*read-suppress*))
              nil)
             (t
              (unless (null escape-ranges)
                (setf escape-ranges (nreverse escape-ranges)))
              (interpret-token client input-stream token escape-ranges))))))))
