(cl:defpackage #:eclector.concrete-syntax-tree
  (:use
   #:cl)

  ;; Result and source location protocol
  (:export
   #:source-position
   #:record-skipped-input)

  ;; Read protocol
  (:export
   #:cst-read)

  ;; Client class (can be used as a superclass)
  (:export
   #:cst-client))
