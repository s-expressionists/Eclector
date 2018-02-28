(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

(test read.smoke
  "Smoke test for the READ function."

  ;; This test focuses on interactions between different parts of the
  ;; reader since the individual parts in isolation are handled by
  ;; more specific tests.
  (map nil (lambda (input-and-expected)
             (destructuring-bind (input expected) input-and-expected
               (flet ((do-it ()
                        (with-input-from-string (stream input)
                          (values (eclector.reader:read stream)
                                  (file-position stream)))))
                 (case expected
                   (t
                    (multiple-value-bind (result position) (do-it)
                      (is (equal expected       result))
                      (is (eql   (length input) position))))))))

       '(("(cons 1 2)" (cons 1 2)))))
