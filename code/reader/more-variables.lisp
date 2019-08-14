(cl:in-package #:eclector.reader)

(defvar *input-stream*)

(defparameter *preserve-whitespace* nil)

(defparameter *client* nil)

(defvar *skip-reason*)

;;; Quasiquote syntax

(defparameter *quasiquote-forbidden* nil)

(defparameter *unquote-forbidden* nil)

(defparameter *backquote-depth* 0)

;;; List syntax

(defvar *consing-dot* '#:|.|)

(defparameter *consing-dot-allowed-p* nil)

(define-condition end-of-list () ())

(defvar *end-of-list* (make-condition 'end-of-list))
