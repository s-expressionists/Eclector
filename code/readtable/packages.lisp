(cl:defpackage #:eclector.readtable
  (:use #:common-lisp)
  ;; Shadowed standard symbols
  (:shadow #:*readtable*
           #:copy-readtable
           #:make-dispatch-macro-character
           #:readtable-case
           #:readtablep
           #:get-macro-character
           #:set-macro-character
           #:get-dispatch-macro-character
           #:set-dispatch-macro-character
           #:set-syntax-from-char)
  (:export #:*readtable*
           #:copy-readtable
           #:copy-readtable-into
           #:make-dispatch-macro-character
           #:readtable-case
           #:readtablep
           #:get-macro-character
           #:set-macro-character
           #:get-dispatch-macro-character
           #:set-dispatch-macro-character
           #:syntax-type
           #:set-syntax-from-char))
