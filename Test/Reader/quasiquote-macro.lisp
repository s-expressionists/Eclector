(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.quasiquote
    :in :eclector.reader)

(test expand-quasiquote.host-equivalence.random
  "Checks equivalence to host's result of expanded and evaluated
   random QUASIQUOTE expressions."

  (let ((*num-trials* 100000)
        (*max-trials* 100000))
    (for-all ((expression (gen-quasiquote-expression)))
      (let ((host-expression (hostify expression)))
        (is (equalp (eval host-expression)
                    (hostify (eval expression))))))))
