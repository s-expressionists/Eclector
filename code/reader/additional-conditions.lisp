(cl:in-package #:eclector.reader)

;;; Type error

(define-condition read-object-type-error (stream-position-reader-error
                                          type-error)
  ())

;;; Conditions related to symbols
;;;
;;; See HyperSpec section 2.3.5 (Valid Patterns for Tokens).

(define-condition package-does-not-exist (stream-position-reader-error)
  ((%package-name :initarg :package-name :reader desired-package-name)))

(define-condition symbol-access-error (stream-position-reader-error)
  ((%symbol-name :initarg :symbol-name :reader desired-symbol-name)
   (%package :initarg :package :reader desired-symbol-package)))

(define-condition symbol-does-not-exist (symbol-access-error)
  ())

(define-condition symbol-is-not-external (symbol-access-error)
  ())

(define-condition symbol-syntax-error (stream-position-reader-error)
  ((%token :initarg :token :reader token)))

(define-condition invalid-constituent-character (symbol-syntax-error)
  ())

(define-condition symbol-name-must-not-be-only-package-markers (symbol-syntax-error)
  ())

(define-condition symbol-name-must-not-end-with-package-marker (symbol-syntax-error)
  ())

(define-condition two-package-markers-must-be-adjacent (symbol-syntax-error)
  ())

(define-condition two-package-markers-must-not-be-first (symbol-syntax-error)
  ())

(define-condition symbol-can-have-at-most-two-package-markers (symbol-syntax-error)
  ())

(define-condition uninterned-symbol-must-not-contain-package-marker (symbol-syntax-error)
  ())

;;; General reader macro conditions

(define-condition sharpsign-invalid (stream-position-reader-error)
  ((%character-found :initarg :character-found :reader character-found)))

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
  (unless *read-suppress*
    (%reader-error stream 'numeric-parameter-not-supplied-but-required
                   :macro-name macro-name)))

;;; Conditions related to strings

(define-condition unterminated-string (missing-delimiter)
  ())

;;; Conditions related to quasiquotation

(defgeneric context-name (context language))

(define-condition backquote-error (stream-position-reader-error)
  ())

(define-condition backquote-context-error (backquote-error)
  ((%context :initarg :context :reader context)))

(define-condition backquote-in-invalid-context (backquote-context-error)
  ())

(define-condition unquote-error (backquote-error)
  ((%splicing-p :initarg :splicing-p :reader splicing-p)))

(define-condition invalid-context-for-unquote (unquote-error)
  ())

(define-condition unquote-not-inside-backquote (invalid-context-for-unquote)
  ())

(define-condition unquote-in-invalid-context (invalid-context-for-unquote
                                              backquote-context-error)
  ())

(define-condition object-must-follow-unquote (unquote-error)
  ())

(define-condition unquote-splicing-in-dotted-list (unquote-error)
  ()
  (:default-initargs
   :splicing-p t))

(define-condition unquote-splicing-at-top (unquote-error)
  ()
  (:default-initargs
   :splicing-p t))

;;; Conditions related to lists

(define-condition unterminated-list (missing-delimiter)
  ())

(define-condition too-many-dots (stream-position-reader-error)
  ())

(define-condition invalid-context-for-consing-dot (stream-position-reader-error)
  ())

(define-condition object-must-follow-consing-dot (stream-position-reader-error)
  ())

(define-condition multiple-objects-following-consing-dot (stream-position-reader-error)
  ())

(define-condition invalid-context-for-right-parenthesis (stream-position-reader-error)
  ())

;;; Conditions related to read-time evaluation

(define-condition read-time-evaluation-inhibited (stream-position-reader-error)
  ())

(define-condition read-time-evaluation-error (stream-position-reader-error)
  ((%expression :initarg :expression :reader expression)
   (%original-condition :initarg :original-condition :reader original-condition)))

;;; Conditions related to characters and numbers

(define-condition unknown-character-name (stream-position-reader-error)
  ((%name :initarg :name :reader name)))

(define-condition digit-expected (stream-position-reader-error)
  ((%character-found :initarg :character-found :reader character-found)
   (%base :initarg :base :reader base)))

(define-condition invalid-radix (stream-position-reader-error)
  ((%radix :initarg :radix :reader radix)))

(define-condition invalid-default-float-format (stream-position-reader-error)
  ((%float-format :initarg :float-format :reader float-format)))

;;; Conditions related to block comments

(define-condition unterminated-block-comment (missing-delimiter)
  ())

;;; Conditions related to arrays

(define-condition unterminated-vector (missing-delimiter)
  ())

(define-condition array-initialization-error (stream-position-reader-error)
  ((%array-type :initarg :array-type :reader array-type)))

(define-condition too-many-elements (array-initialization-error)
  ((%expected-number :initarg :expected-number :reader expected-number)
   (%number-found :initarg :number-found :reader number-found)))

(define-condition no-elements-found (array-initialization-error)
  ((%expected-number :initarg :expected-number :reader expected-number)))

(define-condition incorrect-initialization-length (array-initialization-error)
  ((%axis :initarg :axis :reader axis)
   (%expected-length :initarg :expected-length :reader expected-length)
   (%datum :initarg :datum :reader datum)))

;;; Sharpsign S conditions

(define-condition non-list-following-sharpsign-s (stream-position-reader-error)
  ())

(define-condition no-structure-type-name-found (stream-position-reader-error)
  ())

(define-condition structure-type-name-is-not-a-symbol (read-object-type-error)
  ()
  (:default-initargs
   :expected-type 'symbol))

(define-condition slot-name-is-not-a-symbol (read-object-type-error)
  ()
  (:default-initargs
   :expected-type 'symbol))

(define-condition no-slot-value-found (stream-position-reader-error)
  ((%slot-name :initarg :slot-name
               :reader slot-name)))

;;; Conditions related to feature expressions
;;;
;;; Can be evaluated without a stream context. Therefore each
;;; condition has a stream- and a non-stream-variant.

(define-condition feature-expression-type-error (acclimation:condition type-error)
  ())

(define-condition feature-expression-type-error/reader
    (feature-expression-type-error stream-position-reader-error)
  ())

(define-condition single-feature-expected (acclimation:condition error)
  ((%features :initarg :features :reader features)))

(define-condition single-feature-expected/reader
    (single-feature-expected stream-position-reader-error)
  ())

;;; SHARPSIGN-{EQUALS,SHARPSIGN} conditions

(define-condition reference-error (stream-position-reader-error)
  ((%label :initarg :label :reader label)))

(define-condition sharpsign-equals-label-defined-more-than-once (reference-error)
  ())

(define-condition sharpsign-equals-only-refers-to-self (reference-error)
  ())

(define-condition sharpsign-sharpsign-undefined-label (reference-error)
  ())
