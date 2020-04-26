(cl:defpackage #:eclector.base
  (:use
   #:common-lisp)

  (:shadow
   . #1=(#:end-of-file

         #:read-char))

  (:export
   . #1#)

  ;; Conditions (with accessors)
  (:export
   #:stream-position-reader-error
   #:stream-position

   #:end-of-file

   #:incomplete-construct

   #:missing-delimiter
   #:delimiter)

  ;; Recover restart
  (:export
   #:recover) ; function and restart name

  ;; Exported for eclector.reader, not public use.
  (:export
   #:%reader-error
   #:%recoverable-reader-error

   #:read-char-or-error
   #:read-char-or-recoverable-error))
