(cl:defpackage #:eclector.readtable
  (:use
   #:common-lisp)

  ;; Shadowed standard symbols
  (:shadow
   . #1=(#:*readtable*
         #:readtablep
         #:copy-readtable
         #:make-dispatch-macro-character
         #:readtable-case
         #:get-macro-character
         #:set-macro-character
         #:get-dispatch-macro-character
         #:set-dispatch-macro-character
         #:set-syntax-from-char))

  (:export
   . #1#)

  ;; Conditions
  (:export
   #:unterminated-dispatch-macro
   #:character-must-be-a-dispatching-character
   #:sub-character-must-not-be-a-decimal-digit
   #:unknown-macro-sub-character)

  ;; Other exported symbols
  (:export
   #:copy-readtable-into

   #:syntax-type
   #:syntax-from-char)) ; SETF
