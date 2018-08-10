(cl:defpackage #:eclector.reader
  (:use
   #:common-lisp)

  ;; When the reader is compiled for the purpose of cross compilation,
  ;; we must shadow a certain number of symbols that would otherwise
  ;; clash with the corresponding symbols in the host package
  ;; COMMON-LISP.
  (:shadow
   #:read-char
   #:read
   #:read-preserving-whitespace

   #:end-of-file)

  ;; Contrary to other variables affecting the reader, we cannot use
  ;; the host version of *READTABLE* because we do not necessarily
  ;; use the same representation of readtables as the host does, and
  ;; Common Lisp does not have a standardized API for manipulating
  ;; readtables.  Perhaps we should write a CDR (Common Lisp Document
  ;; Repository) document suggesting such an API.
  (:shadowing-import-from #:eclector.readtable
   #:*readtable*)

  (:import-from #:eclector.base
   #:%reader-error

   #:stream-position-reader-error
   #:stream-position)

  (:import-from #:eclector.readtable
   #:unknown-macro-sub-character)

  (:export
   #:*readtable*

   #:*client*
   #:*skip-reason*
   #:*preserve-whitespace*

   #:read-char
   #:read
   #:read-preserving-whitespace)

  (:export
   #:read-common
   #:read-token
   #:note-skipped-input
   #:interpret-token
   #:interpret-symbol-token
   #:interpret-symbol

   #:call-reader-macro
   #:find-character

   #:evaluate-expression
   #:check-feature-expression
   #:evaluate-feature-expression

   #:fixup)

  ;; Backquote customization.
  (:export
   #:wrap-in-quasiquote
   #:wrap-in-unquote
   #:wrap-in-unquote-splicing)

  ;; Names of additional conditions.
  (:export
   #:end-of-file

   #:backquote-condition
   #:invalid-context-for-backquote
   #:comma-not-inside-backquote
   #:object-must-follow-comma
   #:unquote-splicing-in-dotted-list
   #:unquote-splicing-at-top

   #:too-many-dots
   #:invalid-context-for-consing-dot
   #:object-must-follow-consing-dot
   #:multiple-objects-following-consing-dot
   #:invalid-context-for-right-parenthesis

   #:package-does-not-exist
   #:symbol-does-not-exist
   #:symbol-is-not-external

   #:symbol-name-must-not-be-only-package-markers
   #:symbol-name-must-not-end-with-package-marker
   #:two-package-markers-must-be-adjacent
   #:two-package-markers-must-not-be-first
   #:symbol-can-have-at-most-two-package-markers
   #:uninterned-symbol-must-not-contain-package-marker

   #:unknown-macro-sub-character
   #:numeric-parameter-supplied-but-ignored
   #:numeric-parameter-not-supplied-but-required

   #:read-time-evaluation-inhibited
   #:read-time-evaluation-error

   #:unknown-character-name
   #:digit-expected
   #:invalid-radix
   #:invalid-default-float-format
   #:too-many-elements
   #:no-elements-found
   #:incorrect-initialization-length

   #:feature-expression-type-error
   #:single-feature-expected

   #:sharpsign-invalid
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
   #:set-standard-syntax-and-macros))
