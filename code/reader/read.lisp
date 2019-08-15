(cl:in-package #:eclector.reader)

(defvar *labels*)

(defun read-aux
    (input-stream eof-error-p eof-value recursive-p preserve-whitespace-p)
  (let ((client *client*)
        (*preserve-whitespace* preserve-whitespace-p))
    (if recursive-p
        (read-common client input-stream eof-error-p eof-value)
        (call-as-top-level-read
         client input-stream
         (lambda ()
           (read-common client input-stream eof-error-p eof-value))))))

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
