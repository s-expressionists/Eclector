(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.client
  :in :eclector.concrete-syntax-tree)

;;; Test annotating labeled object references

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

;;; Test wrapper CST classes

(defclass wrapper-cst-client (eclector.concrete-syntax-tree:definition-csts-mixin
                              eclector.concrete-syntax-tree:reference-csts-mixin
                              eclector.concrete-syntax-tree:cst-client)
  ())

(test wrapper-labeled-object-csts/random
  "Random test for reading labeled object expressions into wrapper CSTs."
  (labels ((raw* (cst)
             (typecase cst
               (eclector.concrete-syntax-tree:wrapper-cst
                (raw* (eclector.concrete-syntax-tree:target cst)))
               (t
                (cst:raw cst)))))
    (let ((*test-dribble* (make-broadcast-stream))
          (*num-trials* 10000)
          (*max-trials* 10000))
      (for-all ((expression (gen-labels-and-references)))
        (let* ((input (prin1-to-string expression))
               (result (let ((eclector.base:*client*
                               (make-instance 'wrapper-cst-client)))
                         (eclector.concrete-syntax-tree:read-from-string input))))
          (assert (equal* expression (read-from-string input)))
          (is (equal* expression (cst:raw result)))
          (is (equal* expression (raw* result))))))))
