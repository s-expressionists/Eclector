(cl:in-package #:eclector.reader)

;;; We have provide our own PEEK-CHAR function because CL:PEEK-CHAR
;;; obviously does not use Eclector's readtable.

(defun peek-char (&optional peek-type
                            (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (flet ((done (value)
           (cond ((not (eq value '#1=#.(gensym "EOF")))
                  (return-from peek-char value))
                 (eof-error-p
                  (%reader-error input-stream 'end-of-file))
                 (t
                  (return-from peek-char eof-value)))))
    (if (not (eq peek-type t))
        (done (cl:peek-char peek-type input-stream nil '#1# recursive-p))
        (loop with readtable = *readtable*
              for char = (cl:peek-char nil input-stream nil '#1# recursive-p)
              while (and (not (eq char '#1#))
                         (eq (eclector.readtable:syntax-type readtable char)
                             :whitespace))
              do (read-char input-stream) ; consume whitespace char
              finally (done char)))))

;;;

(defmethod call-reader-macro (client input-stream char readtable)
  (let ((function (eclector.readtable:get-macro-character readtable char)))
    (funcall function input-stream char)))

(defmethod read-common :around (client input-stream eof-error-p eof-value)
  (let ((*input-stream* input-stream))
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
  (let ((readtable *readtable*)
        (token (make-array 10
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0))
        (escape-ranges '())
        (escape-char))
    (labels ((push-char (char)
               (vector-push-extend char token)
               char)
             (start-escape (char)
               (setf escape-char char)
               (push (cons (length token) nil) escape-ranges))
             (end-escape ()
               (setf escape-char nil)
               (setf (cdr (first escape-ranges)) (length token)))
             (read-char-handling-eof (context)
               (let ((char (read-char input-stream nil nil t)))
                 (cond ((not (null char))
                        (values char (eclector.readtable:syntax-type
                                      readtable char)))
                       ((eq context :single-escape)
                        (%recoverable-reader-error
                         input-stream 'unterminated-single-escape-in-symbol
                         :escape-char escape-char
                         :report 'use-partial-symbol)
                        (end-escape)
                        (terminate-token))
                       ((eq context :multiple-escape)
                        (%recoverable-reader-error
                         input-stream 'unterminated-multiple-escape-in-symbol
                         :delimiter escape-char
                         :report 'use-partial-symbol)
                        (end-escape)
                        (terminate-token))
                       (t
                        (terminate-token)))))
             (terminate-token ()
               (return-from read-token
                 (cond (*read-suppress*
                        (note-skipped-input client input-stream
                                            (or *skip-reason* '*read-suppress*))
                        nil)
                       (t
                        (unless (null escape-ranges)
                          (setf escape-ranges (nreverse escape-ranges)))
                        (interpret-token client input-stream token escape-ranges))))))
      (tagbody
         ;; This function is only called when a character is available
         ;; in INPUT-STREAM.
         (multiple-value-bind (char syntax-type) (read-char-handling-eof nil)
           (ecase syntax-type
             (:single-escape
              (start-escape char)
              (push-char (read-char-handling-eof syntax-type))
              (end-escape)
              (go even-escapes))
             (:multiple-escape
              (start-escape char)
              (go odd-escapes))
             (:constituent
              (push-char char)
              (go even-escapes))))
       even-escapes
         (multiple-value-bind (char syntax-type) (read-char-handling-eof nil)
           (ecase syntax-type
             ((:constituent :non-terminating-macro)
              (push-char char)
              (go even-escapes))
             (:single-escape
              (start-escape char)
              (push-char (read-char-handling-eof syntax-type))
              (end-escape)
              (go even-escapes))
             (:multiple-escape
              (start-escape char)
              (go odd-escapes))
             (:terminating-macro
              (unread-char char input-stream)
              (terminate-token))
             (:whitespace
              (when *preserve-whitespace*
                (unread-char char input-stream))
              (terminate-token))))
       odd-escapes
         (multiple-value-bind (char syntax-type)
             (read-char-handling-eof :multiple-escape)
           (ecase syntax-type
             ((:constituent :terminating-macro
               :non-terminating-macro :whitespace)
              (push-char char)
              (go odd-escapes))
             (:single-escape
              (push-char (read-char-handling-eof syntax-type))
              (go odd-escapes))
             (:multiple-escape
              (end-escape)
              (go even-escapes))))))))
