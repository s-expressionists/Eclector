(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.client
  :in :eclector.concrete-syntax-tree)

(defclass annotating-cst-client (eclector.reader.test::label-reference-annotation-mixin
                                 eclector.concrete-syntax-tree:cst-client)
  ())

(test labeled-object-annotation
  "Test custom labeled object reference processing."
  (is (equal* '(a #1=(b (:circular-reference #1#)
                      c (:another-circular-reference #1#)
                      d)
                e (:ordinary-reference #1#) f)
              (let ((eclector.base:*client*
                      (make-instance 'annotating-cst-client)))
                (cst:raw (eclector.concrete-syntax-tree:read-from-string
                          "(A #1=(b #1# c #1# d) e #1# f)"))))))
