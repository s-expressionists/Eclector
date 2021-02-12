(defpackage #:eclector.syntax-extensions.object-in-package.test
  (:use
   #:cl
   #:fiveam))

(in-package #:eclector.syntax-extensions.object-in-package.test)

(def-suite* :eclector.extensions.object-in-package
  :in :eclector.extensions)

(test smoke
  "Smoke test for the object-in-package syntax extension."
  (let ((eclector.base:*client* (make-instance 'eclector.syntax-extensions.object-in-package:object-in-package-syntax-mixin)))
    (multiple-value-bind (value position)
        (eclector.reader:read-from-string "cl-user::(foo bar)")
      (is (equal '(cl-user::foo cl-user::bar) value))
      (is (eql 18 position)))))
