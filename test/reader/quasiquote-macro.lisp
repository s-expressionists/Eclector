(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.quasiquote
    :in :eclector.reader)

(test expand-quasiquote.smoke
  "Smoke test for QUASIQUOTE expansion. This covers some of the error
   cases the random test cannot."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected) input-and-expected
            (let ((form (with-input-from-string (stream input)
                          (eclector.reader:read stream))))
              (flet ((do-it ()
                       (macroexpand-1 form)))
                (case expected
                  (eclector.reader:undefined-use-of-backquote
                   (signals eclector.reader:undefined-use-of-backquote
                     (do-it)))
                  (eclector.reader:unquote-splicing-in-dotted-list
                   (signals eclector.reader:unquote-splicing-in-dotted-list
                     (do-it)))
                  (t
                   (is (equalp expected (eval (do-it))))))))))
        '(("`,1"           1)
          ("`,@1"          eclector.reader:undefined-use-of-backquote)

          ("`(1 ,2)"       (1 2))
          ("`(1 ,@'(2))"   (1 2))

          ("`(1 . ,2)"     (1 . 2))
          ("`(1 . ,@'(2))" eclector.reader:unquote-splicing-in-dotted-list)

          ("`#(,1)"        #(1))
          ("`#(,@'(1))"    #(1)))))

(test expand-quasiquote.host-equivalence.random
  "Checks equivalence to host's result of expanded and evaluated
   random QUASIQUOTE expressions."

  (let () #+no ((*num-trials* 100000)
        (*max-trials* 100000))
    (for-all ((expression (gen-quasiquote-expression)))
      (let ((host-expression (hostify expression)))
        (is (equalp (eval host-expression)
                    (hostify (eval expression))))))))
