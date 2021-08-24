(cl:in-package #:eclector.base)

#+sbcl
(defun &optional-and-&key-style-warning-p (condition)
  (and (typep condition 'simple-condition)
       (eql 0 (search "&OPTIONAL and &KEY found in the same lambda list"
                      (simple-condition-format-control condition)))))

#+sbcl
(deftype &optional-and-&key-style-warning ()
  '(satisfies &optional-and-&key-style-warning-p))
