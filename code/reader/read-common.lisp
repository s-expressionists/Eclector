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

;;; Establishing context

(defmethod read-common (client input-stream eof-error-p eof-value)
  (let ((readtable *readtable*))
    (tagbody
     step-1-start
       (let ((char (read-char input-stream eof-error-p)))
         (when (null char)
           (return-from read-common eof-value))
         (case (eclector.readtable:syntax-type readtable char)
           (:whitespace
            (go step-1-start))
           ((:terminating-macro :non-terminating-macro)
            ;; There is no need to consider the value of EOF-ERROR-P
            ;; in reader macros since: "read signals an error of type
            ;; end-of-file, regardless of eof-error-p, if the file
            ;; ends in the middle of an object representation."
            ;; (HyperSpec entry for READ)
            (let* ((*skip-reason* nil)
                   (values (multiple-value-list
                            (call-reader-macro
                             client input-stream char readtable))))
              (cond ((null values)
                     (note-skipped-input client input-stream
                                         (or *skip-reason* :reader-macro))
                     (go step-1-start))
                    ;; This case takes care of reader macro not
                    ;; returning nil when *READ-SUPPRESS* is true.
                    (*read-suppress*
                     (note-skipped-input client input-stream
                                         (or *skip-reason* '*read-suppress*))
                     (return-from read-common nil))
                    (t
                     (return-from read-common (first values))))))
           (t
            (unread-char char input-stream)
            (return-from read-common
              (let ((*skip-reason* nil))
                (read-token client input-stream eof-error-p eof-value)))))))))
