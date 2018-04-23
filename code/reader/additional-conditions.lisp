(cl:in-package #:eclector.reader)

(defun %reader-error (stream datum &rest arguments)
  (let ((position (ignore-errors (file-position stream))))
    (apply #'error datum (append arguments
                                 (list :stream stream)
                                 (when position
                                   (list :stream-position position))))))

(define-condition stream-position-reader-error (acclimation:condition reader-error)
  ((%stream-position :initarg :stream-position
                     :reader stream-position)))

;;; Conditions related to quasiquotation

(define-condition backquote-condition (stream-position-reader-error)
  ())

(define-condition invalid-context-for-backquote (backquote-condition)
  ())

(define-condition comma-not-inside-backquote (backquote-condition)
  ())

(define-condition unquote-splicing-in-dotted-list (backquote-condition)
  ())

(define-condition unquote-splicing-at-top (backquote-condition)
  ())

;;; Conditions related to consing dot

(define-condition invalid-context-for-consing-dot (stream-position-reader-error)
  ())

(define-condition consing-dot-most-be-followed-by-object (stream-position-reader-error)
  ())

(define-condition multiple-objects-following-consing-dot (stream-position-reader-error)
  ())

(define-condition invalid-context-for-right-parenthesis (stream-position-reader-error)
  ())

(define-condition sub-char-must-not-be-a-decimal-digit (acclimation:condition error)
  ((%disp-char :initarg :disp-char :reader disp-char)
   (%sub-char :initarg :sub-char :reader sub-char)))

(define-condition char-must-be-a-dispatching-character (acclimation:condition error)
  ((%disp-char :initarg :disp-char :reader disp-char)))

(define-condition symbol-access-error (stream-position-reader-error)
  ((%symbol-name :initarg :symbol-name :reader desired-symbol-name)
   (%package :initarg :package :reader desired-symbol-package)))

(define-condition symbol-does-not-exist (symbol-access-error)
  ())

(define-condition symbol-is-not-external (symbol-access-error)
  ())

(define-condition symbol-syntax-error (stream-position-reader-error)
  ((%token :initarg :token :reader token)))

(define-condition symbol-name-must-not-end-with-package-marker (symbol-syntax-error)
  ())

(define-condition two-package-markers-must-be-adjacent (symbol-syntax-error)
  ())

(define-condition two-package-markers-must-not-be-first (symbol-syntax-error)
  ())

(define-condition symbol-can-have-at-most-two-package-markers (symbol-syntax-error)
  ())

(define-condition unknown-macro-sub-character (stream-position-reader-error)
  ((%sub-char :initarg :sub-char :reader sub-char)))

(define-condition numeric-parameter-supplied-but-ignored (stream-position-reader-error)
  ((%parameter :initarg :parameter :reader parameter)
   (%macro-name :initarg :macro-name :reader macro-name)))

(defun numeric-parameter-ignored (stream macro-name parameter)
  (unless *read-suppress*
    (%reader-error stream 'numeric-parameter-supplied-but-ignored
                   :parameter parameter :macro-name macro-name)))

(define-condition numeric-parameter-not-supplied-but-required (stream-position-reader-error)
  ((%macro-name :initarg :macro-name :reader macro-name)))

(defun numeric-parameter-not-supplied (stream macro-name)
  (%reader-error stream 'numeric-parameter-not-supplied-but-required
                 :macro-name macro-name))

(define-condition read-time-evaluation-inhibited (stream-position-reader-error)
  ())

(define-condition unknown-character-name (stream-position-reader-error)
  ((%name :initarg :name :reader name)))

(define-condition digit-expected (stream-position-reader-error)
  ((%character-found :initarg :character-found :reader character-found)
   (%base :initarg :base :reader base)))

(define-condition invalid-radix (stream-position-reader-error)
  ((%radix :initarg :radix :reader radix)))

(define-condition invalid-default-float-format (stream-position-reader-error)
  ((%float-format :initarg :float-format :reader float-format)))

(define-condition too-many-elements (stream-position-reader-error)
  ((%expected-number :initarg :expected-number :reader expected-number)
   (%number-found :initarg :number-found :reader number-found)))

(define-condition no-elements-found (stream-position-reader-error)
  ((%expected-number :initarg :expected-number :reader expected-number)))

(define-condition incorrect-initialization-length (acclimation:condition error)
  ((%expected-length :initarg :expected-length :reader expected-length )
   (%datum :initarg :datum :reader datum)))

(define-condition single-feature-expected (acclimation:condition error)
  ((%features :initarg :features :reader features)))

(define-condition sharpsign-invalid (stream-position-reader-error)
  ((%character-found :initarg :character-found :reader character-found)))

(define-condition sharpsign-equals-label-defined-more-than-once (stream-position-reader-error)
  ((%label :initarg :label :reader label)))

(define-condition sharpsign-sharpsign-undefined-label (stream-position-reader-error)
  ((%label :initarg :label :reader label)))
