(cl:defpackage #:eclector.reader
  (:use
   #:common-lisp)

  ;; When the reader is compiled for the purpose of cross compilation,
  ;; we must shadow a certain number of symbols that would otherwise
  ;; clash with the corresponding symbols in the host package
  ;; COMMON-LISP.
  (:shadow
   #:peek-char
   #:read
   #:read-preserving-whitespace
   #:read-from-string
   #:read-delimited-list)

  (:shadowing-import-from #:eclector.base
   #:end-of-file

   #:read-char

   #:recover) ; Restart name

  (:import-from #:eclector.base
   #:%reader-error

   #:%recoverable-reader-error
   #:recovery-description

   #:stream-position-reader-error
   #:stream-position

   #:incomplete-construct

   #:missing-delimiter
   #:delimiter)

  ;; Contrary to other variables affecting the reader, we cannot use
  ;; the host version of *READTABLE* because we do not necessarily
  ;; use the same representation of readtables as the host does, and
  ;; Common Lisp does not have a standardized API for manipulating
  ;; readtables.  Perhaps we should write a CDR (Common Lisp Document
  ;; Repository) document suggesting such an API.
  (:shadowing-import-from #:eclector.readtable
   #:*readtable*)

  (:import-from #:eclector.readtable
   #:unterminated-dispatch-macro
   #:unknown-macro-sub-character)

  (:export
   #:*readtable*

   #:*client*
   #:*skip-reason*
   #:*preserve-whitespace*

   #:read-char
   #:peek-char
   #:read
   #:read-preserving-whitespace
   #:read-from-string
   #:read-delimited-list

   #:recover) ; Restart name

  ;; Client protocol
  (:export
   #:read-common
   #:read-token
   #:note-skipped-input
   #:interpret-token
   #:interpret-symbol-token
   #:interpret-symbol

   #:call-reader-macro
   #:find-character
   #:make-structure-instance

   #:call-with-current-package

   #:evaluate-expression
   #:check-feature-expression
   #:evaluate-feature-expression

   #:fixup)

  ;; Quote and backquote customization.
  (:export
   #:wrap-in-quote

   #:wrap-in-quasiquote
   #:wrap-in-unquote
   #:wrap-in-unquote-splicing)

  ;; Conditions related to symbol and end-of-input.
  (:export
   #:end-of-file

   #:incomplete-construct

   #:missing-delimiter
   #:delimiter

   #:read-object-type-error

   #:package-does-not-exist
   #:symbol-does-not-exist
   #:symbol-is-not-external

   #:invalid-constituent-character
   #:symbol-name-must-not-be-only-package-markers
   #:symbol-name-must-not-end-with-package-marker
   #:two-package-markers-must-be-adjacent
   #:two-package-markers-must-not-be-first
   #:symbol-can-have-at-most-two-package-markers
   #:uninterned-symbol-must-not-contain-package-marker)

  ;; Conditions related to reader macros.
  (:export
   #:sharpsign-invalid

   #:unterminated-dispatch-macro
   #:unknown-macro-sub-character

   #:numeric-parameter-supplied-but-ignored
   #:numeric-parameter-not-supplied-but-required

   #:unterminated-string

   #:backquote-error
   #:backquote-context-error
   #:backquote-in-invalid-context
   #:unquote-error
   #:invalid-context-for-unquote
   #:unquote-not-inside-backquote
   #:unquote-in-invalid-context
   #:object-must-follow-unquote
   #:unquote-splicing-in-dotted-list
   #:unquote-splicing-at-top

   #:unterminated-list
   #:too-many-dots
   #:invalid-context-for-consing-dot
   #:object-must-follow-consing-dot
   #:multiple-objects-following-consing-dot
   #:invalid-context-for-right-parenthesis

   #:read-time-evaluation-inhibited
   #:read-time-evaluation-error

   #:unknown-character-name
   #:digit-expected
   #:invalid-radix
   #:invalid-default-float-format

   #:unterminated-block-comment

   #:unterminated-vector
   #:too-many-elements
   #:no-elements-found
   #:incorrect-initialization-length

   #:non-list-following-sharpsign-s
   #:no-structure-type-name-found
   #:structure-type-name-is-not-a-symbol
   #:slot-name-is-not-a-string-designator
   #:no-slot-value-found

   #:feature-expression-type-error
   #:single-feature-expected

   #:sharpsign-equals-label-defined-more-than-once
   #:sharpsign-equals-only-refers-to-self
   #:sharpsign-sharpsign-undefined-label)

  ;; Names of macros related to backquote.  We export them so that the
  ;; pretty printer can use them properly.
  (:export
   #:quasiquote
   #:unquote
   #:unquote-splicing)

  ;; Readtable initialization
  (:export
   #:set-standard-syntax-types
   #:set-standard-macro-characters
   #:set-standard-dispatch-macro-characters
   #:set-standard-syntax-and-macros)

  ;; Reader macro functions
  (:export
   #:sharpsign-single-quote/relaxed))
