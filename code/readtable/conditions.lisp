(cl:in-package #:eclector.readtable)

(define-condition sub-char-must-not-be-a-decimal-digit (acclimation:condition error)
  ((%disp-char :initarg :disp-char :reader disp-char)
   (%sub-char :initarg :sub-char :reader sub-char)))

(define-condition char-must-be-a-dispatching-character (acclimation:condition error)
  ((%disp-char :initarg :disp-char :reader disp-char)))

(define-condition unknown-macro-sub-character (eclector.base:stream-position-reader-error)
  ((%disp-char :initarg :disp-char :reader disp-char)
   (%sub-char :initarg :sub-char :reader sub-char)))
