(cl:in-package #:eclector.concrete-syntax-tree.test)

(defmacro both ((operator cst))
  (let ((cst-operator (find-symbol (symbol-name operator)
                                   (find-package '#:cst))))
    (alexandria:once-only (cst)
      `(values (,cst-operator ,cst) (,operator (cst:raw ,cst))))))

(defun is-consistent-with-raw (cst)
  (let ((seen (make-hash-table :test #'eq))
        (atom->cst (make-hash-table :test #'eq)))
    (labels ((rec (cst)
               (if (gethash cst seen)
                   (return-from rec)
                   (setf (gethash cst seen) t))
               (if (typep cst 'eclector.concrete-syntax-tree:wrapper-cst)
                   (rec (eclector.concrete-syntax-tree:target cst))
                   (multiple-value-bind (cst-atom-p raw-atom-p)
                       (both (atom cst))
                     (is (eq raw-atom-p cst-atom-p))
                     (if raw-atom-p
                         (let ((raw-atom (cst:raw cst)))
                           ;; For atoms like pathnames, instances of
                           ;; structure classes or instances of
                           ;; standard classes, we expect multiple
                           ;; `eq' occurrences in the raw expression
                           ;; to have `eq' corresponding CSTs.
                           ;;
                           ;; We may have to refine this later since
                           ;; something like
                           ;;
                           ;;   (is-consistent-with-raw
                           ;;    (let ((* #P"foo"))
                           ;;      (eclector.concrete-syntax-tree:read-from-string
                           ;;       "(#.* #.*)")))
                           ;;
                           ;; would currently result in a false positive.
                           (unless (typep raw-atom '(or number
                                                        character
                                                        symbol))
                             (multiple-value-bind (existing-cst foundp)
                                 (gethash raw-atom atom->cst)
                               (if foundp
                                   (is (eq cst existing-cst))
                                   (setf (gethash raw-atom atom->cst) cst)))))
                         (multiple-value-bind (cst-car raw-car)
                             (both (first cst))
                           (multiple-value-bind (cst-cdr raw-cdr)
                               (both (rest cst))
                             (is (eq raw-car (cst:raw cst-car)))
                             (is (eq raw-cdr (cst:raw cst-cdr)))
                             (rec cst-car)
                             (rec cst-cdr))))))))
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
