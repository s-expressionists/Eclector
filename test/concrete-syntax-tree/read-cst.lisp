(cl:in-package #:eclector.concrete-syntax-tree.test)

(in-suite :eclector.concrete-syntax-tree)

(test read-cst/smoke
  "Smoke test for the READ-CST function."

  (finishes (with-input-from-string (stream "(cons 1 2)")
              (eclector.concrete-syntax-tree:cst-read stream))))
