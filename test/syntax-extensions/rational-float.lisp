(cl:defpackage #:eclector.syntax-extensions.rational-float.test
  (:use
   #:cl
   #:fiveam))

(cl:in-package #:eclector.syntax-extensions.rational-float.test)

(def-suite* :eclector.syntax-extensions.rational-float
  :in :eclector.syntax-extensions)

(test smoke
  "TODO"
  (let ((client (make-instance 'eclector.syntax-extensions.rational-float:client)))
    (let ((eclector.base:*client* client))
      (is (= 617/5 (eclector.reader:read-from-string "1.234R2"))))

    ;; TODO: this fails at the moment because when we read "E" there
    ;; is no extensible way to
    (let ((eclector.base:*client* client))
      (is (eql 617/5
               (eclector.reader:call-with-state-value
                eclector.base:*client*
                (lambda () (eclector.reader:read-from-string "1.234E2"))
                '*read-default-float-format* :custom))))))
