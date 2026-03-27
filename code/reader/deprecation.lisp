(cl:in-package #:eclector.reader)

;;; Replaced by reader state protocol

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

;;; Calling reader macros and behavior of standard reader macros

(defgeneric make-structure-instance (client name initargs))

#+sbcl
(declaim (sb-ext:deprecated
          :early ("Eclector" "0.12")
          (function make-structure-instance :replacement make-literal)))

;;; Creating s-expressions; replaced by `make-expression'

(defgeneric wrap-in-function (client name)
  (:method ((client t) (name t))
    (list 'function name)))

(defgeneric wrap-in-quote (client material)
  (:method ((client t) (material t))
    (list 'quote material)))

(defgeneric wrap-in-quasiquote (client form)
  (:method ((client t) (form t))
    (list 'quasiquote form)))

(defgeneric wrap-in-unquote (client form)
  (:method ((client t) (form t))
    (list 'unquote form)))

(defgeneric wrap-in-unquote-splicing (client form)
  (:method ((client t) (form t))
    (list 'unquote-splicing form)))

#+sbcl
(declaim (sb-ext:deprecated
          :early ("Eclector" "0.12")
          (function wrap-in-function :replacement make-expression)
          (function wrap-in-quote :replacement make-expression)
          (function wrap-in-quasiquote :replacement make-expression)
          (function wrap-in-unquote :replacement make-expression)
          (function wrap-in-unquote-splicing :replacement make-expression)))
