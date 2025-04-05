(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.labeled-objects
  :in :eclector.concrete-syntax-tree)

(test labeled-objects/random
  "Random test for reading labeled object expressions into CSTs.

Test with the ordinary CST client and a client that creates additional
CSTs for labeled object definitions and references."
  (labels ((raw* (cst)
             (typecase cst
               (eclector.concrete-syntax-tree:wrapper-cst
                (raw* (eclector.concrete-syntax-tree:target cst)))
               (t
                (cst:raw cst))))
           (do-client (client)
             (let ((*test-dribble* (make-broadcast-stream)) ; too much output otherwise
                   (*num-trials* 10000)
                   (*max-trials* 10000))
               (for-all ((expression (gen-labels-and-references)))
                 (let* ((input (let ((*print-circle* t))
                                 (prin1-to-string expression)))
                        (result (let ((eclector.base:*client* client))
                                  (eclector.concrete-syntax-tree:read-from-string
                                   input))))
                   (assert (equal* expression (read-from-string input)))
                   (expect input "cst:raw" (equal* expression (cst:raw result)))
                   (expect input "raw*"    (equal* expression (raw* result)))
                   (is-true (valid-cst-parse-result-p
                             eclector.concrete-syntax-tree::*cst-client* result)
                            "~@<For input ~S, the result CST ~S is not valid~@:>"
                            input result)
                   (is-consistent-with-raw result))))))
    (do-client eclector.concrete-syntax-tree::*cst-client*)
    (do-client (make-instance 'wrapper-cst-client))))
