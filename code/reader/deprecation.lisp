(cl:in-package #:eclector.reader)

;;; Replaced by reader state protocol

(defgeneric call-with-current-package (client thunk package-designator)
  (:method ((client t) (thunk t) (package-designator t))
    (let ((*package* (find-package package-designator)))
      (funcall thunk))))
#+sbcl (declaim (sb-ext:deprecated
                 :early "Eclector 0.9"
                 (function call-with-current-package
                           :replacement call-with-state-value)))

;;; Creating s-expressions; replaced by `make-expression'

(defgeneric wrap-in-function (client name)
  (:method (client name)
    (declare (ignore client))
    (list 'function name)))

(defgeneric wrap-in-quote (client material)
  (:method (client material)
    (declare (ignore client))
    (list 'quote material)))

(defgeneric wrap-in-quasiquote (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'quasiquote form)))

(defgeneric wrap-in-unquote (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'unquote form)))

(defgeneric wrap-in-unquote-splicing (client form)
  (:method (client form)
    (declare (ignore client))
    (list 'unquote-splicing form)))

#+sbcl (declaim (sb-ext:deprecated
                 :early "Eclector 0.10"
                 (function wrap-in-function :replacement make-expression)
                 (function wrap-in-quote :replacement make-expression)
                 (function wrap-in-quasiquote :replacement make-expression)
                 (function wrap-in-unquote :replacement make-expression)
                 (function wrap-in-unquote-splicing :replacement make-expression)))
