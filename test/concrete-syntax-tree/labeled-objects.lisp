(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.labeled-objects
  :in :eclector.concrete-syntax-tree)

(test labeled-objects/random
  "Random test for reading labeled object expressions into CSTs."
  (let ((*test-dribble* (make-broadcast-stream)) ; too much output otherwise
        (*num-trials* 10000)
        (*max-trials* 10000))
    (for-all ((expression (gen-labels-and-references)))
      (let* ((input (let ((*print-circle* t))
                      (prin1-to-string expression)))
             (result (eclector.concrete-syntax-tree:read-from-string input)))
        (assert (equal* expression (read-from-string input)))
        (is (equal* expression (cst:raw result)))))))
