(cl:in-package #:eclector.concrete-syntax-tree.test)

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
                       (is (eq raw-car (cst:raw cst-car)))
                       (is (eq raw-cdr (cst:raw cst-cdr)))
                       (rec cst-car)
                       (rec cst-cdr)))))))
      (rec cst))))

(defun valid-cst-parse-result-p (client root-cst)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rec (cst)
               (cond ((gethash cst seen)
                      t)
                     (t
                      (setf (gethash cst seen) t)
                      (typecase cst
                        (cst:atom-cst
                         (not (eclector.reader:labeled-object-state
                               client (cst:raw cst))))
                        (cst:cons-cst
                         (and (rec (cst:first cst))
                              (rec (cst:rest cst))))
                        (eclector.concrete-syntax-tree:wrapper-cst
                         (rec (eclector.concrete-syntax-tree:target cst))))))))
      (rec root-cst))))
