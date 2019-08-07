(cl:in-package #:eclector.reader)

(defvar *input-stream*)

(defparameter *backquote-allowed-p* t)

(defparameter *backquote-in-subforms-allowed-p* t)

(defparameter *backquote-forbidden* nil)

(defparameter *unquote-forbidden* nil)

(defparameter *backquote-depth* 0)

(defvar *consing-dot* '#:|.|)

(defparameter *consing-dot-allowed-p* nil)

(define-condition end-of-list () ())

(defvar *end-of-list* (make-condition 'end-of-list))

(defparameter *preserve-whitespace* nil)

(defparameter *client* nil)

(defvar *skip-reason*)
