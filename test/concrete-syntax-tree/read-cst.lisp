(cl:in-package #:eclector.concrete-syntax-tree.test)

(in-suite :eclector.concrete-syntax-tree)

(defmacro both ((operator cst))
  (let ((cst-operator (find-symbol (symbol-name operator)
                                   (find-package '#:cst))))
    (alexandria:once-only (cst)
      `(values (,cst-operator ,cst) (,operator (cst:raw ,cst))))))

(defun is-consistent-with-raw (cst)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (cst)
               (if (gethash cst seen)
                   (return-from rec)
                   (setf (gethash cst seen) t))
               (multiple-value-bind (cst-atom-p raw-atom-p) (both (atom cst))
                 (is (eq raw-atom-p cst-atom-p))
                 (unless raw-atom-p
                   (multiple-value-bind (cst-car raw-car) (both (first cst))
                     (multiple-value-bind (cst-cdr raw-cdr) (both (rest cst))
                       (is (equal raw-car (cst:raw cst-car)))
                       (is (equal raw-cdr (cst:raw cst-cdr)))
                       (rec cst-car)
                       (rec cst-cdr)))))))
      (rec cst))))

(test read-cst/smoke
  "Smoke test for the READ-CST function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected-raw &optional expected-location)
              input-and-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.concrete-syntax-tree:cst-read stream)
                               (file-position stream)))))
              (case expected-raw
                (t
                 (multiple-value-bind (result position) (do-it)
                   ;; CST result and its raw content.
                   (is (typep result 'cst:cst))
                   (is-consistent-with-raw result)
                   (let ((raw (cst:raw result)))
                     (is (equal expected-raw raw)))
                   ;; Expected source location.
                   (is (equal expected-location (cst:source result)))
                   ;; Consumed all input.
                   (is (eql (length input) position))))))))

        '(("(cons 1 2)"    (cons 1 2) ( 0 . 10))
          ("#+(or) `1 2"   2          (10 . 11))
          ("#|comment|# 1" 1          (12 . 13)))))
