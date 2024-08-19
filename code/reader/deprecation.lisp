(cl:in-package #:eclector.reader)

(defgeneric call-with-current-package (client thunk package-designator)
  (:method ((client t) (thunk t) (package-designator t))
    (let ((*package* (find-package package-designator)))
      (funcall thunk))))
#+sbcl (declaim (sb-ext:deprecated
                 :early ("Eclector" "0.9")
                 (function call-with-current-package
                           :replacement call-with-state-value)))

(defmacro with-forbidden-quasiquotation
    ((context &optional (quasiquote-forbidden-p t)
                        (unquote-forbidden-p t))
     &body body)
  `(with-quasiquotation-state (eclector.base:*client*
                               ,context
                               ,quasiquote-forbidden-p
                               ,unquote-forbidden-p)
     ,@body))
#+sbcl (declaim (sb-ext:deprecated
                 :early ("Eclector" "0.11")
                 (function with-forbidden-quasiquotation
                           :replacement with-quasiquotation-state)))
