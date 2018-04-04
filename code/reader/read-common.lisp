(cl:in-package #:eclector.reader)

(defmethod call-reader-macro (function input-stream char)
  (funcall (eclector.readtable:get-macro-character *readtable* char)
           input-stream
           char))

(defmethod read-common :around (client input-stream eof-error-p eof-value)
  (let ((*backquote-allowed-p* *backquote-in-subforms-allowed-p*)
        (*backquote-in-subforms-allowed-p* nil))
    (call-next-method)))

(defmethod read-common (client input-stream eof-error-p eof-value)
  (tagbody
   step-1-start
     (let ((*skip-reason* nil)
           (char (read-char input-stream nil nil)))
       (when (null char)
         (if eof-error-p
             (error 'end-of-file :stream input-stream)
             (return-from read-common eof-value)))
       (case (eclector.readtable:syntax-type *readtable* char)
         (:whitespace
          (go step-1-start))
         ((:terminating-macro :non-terminating-macro)
          (let ((values (multiple-value-list
                         (call-reader-macro
                          (eclector.readtable:get-macro-character *readtable* char)
                          input-stream
                          char))))
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
        (token-escapes (make-array 100
                                   :adjustable t
                                   :fill-pointer 0)))
    (flet ((push-char (char escapesp)
             (vector-push-extend char token)
             (vector-push-extend escapesp token-escapes)
             char)
           (read-char-handling-eof ()
             (let ((char (read-char input-stream nil nil)))
               (cond ((not (null char))
                      char)
                     (eof-error-p
                      (error 'end-of-file :stream input-stream))
                     (t
                      (return-from read-token eof-value))))))
      (tagbody
         ;; This function is only called when a character is available
         ;; in INPUT-STREAM.
         (let ((char (read-char input-stream)))
           (ecase (eclector.readtable:syntax-type *readtable* char)
             (:single-escape
              (push-char (read-char-handling-eof) t)
              (go step-8-even-escapes))
             (:multiple-escape
              (go step-9-odd-escapes))
             (:constituent
              (push-char char nil)
              (go step-8-even-escapes))))
       step-8-even-escapes
         (let ((char (read-char input-stream nil nil)))
           (when (null char)
             (go step-10-terminate-token))
           (ecase (eclector.readtable:syntax-type *readtable* char)
             ((:constituent :non-terminating-macro)
              (push-char char nil)
              (go step-8-even-escapes))
             (:single-escape
              (push-char (read-char-handling-eof) t)
              (go step-8-even-escapes))
             (:multiple-escape
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
              (push-char char t)
              (go step-9-odd-escapes))
             (:single-escape
              (push-char (read-char-handling-eof) t)
              (go step-9-odd-escapes))
             (:multiple-escape
              (go step-8-even-escapes))))
       step-10-terminate-token
         (return-from read-token
           (cond
             (*read-suppress*
              (note-skipped-input client input-stream
                                  (or *skip-reason* '*read-suppress*))
              nil)
             (t
              (interpret-token token token-escapes input-stream))))))))
