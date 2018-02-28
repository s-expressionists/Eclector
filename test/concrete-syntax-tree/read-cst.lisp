(cl:in-package #:eclector.concrete-syntax-tree.test)

(in-suite :eclector.concrete-syntax-tree)

(test read-cst/smoke
  "Smoke test for the READ-CST function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected) input-and-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.concrete-syntax-tree:cst-read stream)
                               (file-position stream)))))
              (case expected
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (typep result 'cst:cst))
                   (is (eql (length input) position))))))))

        '(("(cons 1 2)"  t)
          ("#+(or) `1 2" t))))
