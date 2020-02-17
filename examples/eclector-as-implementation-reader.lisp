(cl:defpackage #:eclector.examples.as-implementation-reader.cl
  (:use
   #:cl)

  (:shadow
   . #1=(#:copy-readtable
         #:make-dispatch-macro-character
         #:readtable-case
         #:readtablep
         #:set-syntax-from-char
         #:get-macro-character
         #:set-macro-character
         #:get-dispatch-macro-character
         #:set-dispatch-macro-character

         #:read
         #:read-preserving-whitespace
         #:read-from-string
         #:read-delimited-list))

  (:shadowing-import-from #:eclector.readtable
   #:*readtable*)

  (:export
   #:*readtable*

   . #1#)

  (:local-nicknames
   (#:readtable #:eclector.readtable)
   (#:reader    #:eclector.reader))

  (:documentation
   "This package illustrates how the implementation could set up its
CL package."))

(cl:in-package #:eclector.examples.as-implementation-reader.cl)

;;; Stream functions

;;; The implementation must provide
;;  CL:PEEK-CHAR
;;; CL:READ-CHAR
;;; CL:UNREAD-CHAR

;;; Readtable interface

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  (if to-readtable
      (readtable:copy-readtable-into from-readtable to-readtable)
      (readtable:copy-readtable from-readtable)))

(defun readtablep (object)
  (readtable:readtablep object))

(defun set-syntax-from-char (to-char from-char &optional (to-readtable *readtable*) (from-readtable eclector.reader::*standard-readtable*))
  (readtable:set-syntax-from-char to-char from-char to-readtable from-readtable))

(defun get-macro-character (char &optional (readtable *readtable*))
  (readtable:get-macro-character readtable char))

(defun set-macro-character (char function &optional non-terminating-p (readtable *readtable*))
  (readtable:set-macro-character readtable char function non-terminating-p))

(defun get-dispatch-macro-character (disp-char sub-char &optional (readtable *readtable*))
  (readtable:get-dispatch-macro-character readtable disp-char sub-char))

(defun set-dispatch-macro-character (disp-char sub-char function &optional (readtable *readtable*))
  (readtable:set-dispatch-macro-character readtable disp-char sub-char function))

;;; Reader interface

(defun read (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (reader:read input-stream eof-error-p eof-value recursive-p))

(defun read-preserving-whitespace (&optional (input-stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (reader:read-preserving-whitespace input-stream eof-error-p eof-value recursive-p))

(defun read-from-string (string &optional (eof-error-p t) eof-value
                                &key (start 0) end preserving-whitespace)
  (reader:read-from-string string eof-error-p eof-value
                           :start start :end end
                           :preserve-whitespace preserving-whitespace))

(defun read-delimited-list (char &optional (input-stream *standard-input*) recursive-p)
  (reader:read-delimited-list char input-stream recursive-p))

(cl:defpackage #:eclector.examples.as-implementation-reader.test
  (:use
   #:cl
   #:eclector.examples.as-implementation-reader.cl)

  (:shadowing-import-from #:eclector.examples.as-implementation-reader.cl
   #:*readtable*

   #:copy-readtable
   #:make-dispatch-macro-character
   #:readtable-case
   #:readtablep
   #:set-syntax-from-char
   #:get-macro-character
   #:set-macro-character
   #:get-dispatch-macro-character
   #:set-dispatch-macro-character

   #:read
   #:read-preserving-whitespace
   #:read-from-string
   #:read-delimited-list))

(cl:in-package #:eclector.examples.as-implementation-reader.test)

(let ((*readtable* (copy-readtable *readtable*)))
  (set-syntax-from-char #\] #\))
  (set-macro-character #\[ (lambda (stream char)
                             (prog1
                                 (read stream)
                               (assert (char= #\] (read-char stream))))))
  (read-from-string "[:foo]"))
