(cl:in-package #:eclector.concrete-syntax-tree.test)

(defmacro both ((operator cst))
  (let ((cst-operator (find-symbol (symbol-name operator)
                                   (find-package '#:cst))))
    (alexandria:once-only (cst)
      `(values (,cst-operator ,cst) (,operator (cst:raw ,cst))))))

(defun is-consistent-with-raw (cst)
  (let* ((seen (make-hash-table :test #'eq))
         (atom->cst (make-hash-table :test #'eq))
         (tail (list cst))
         (worklist tail))
    (labels ((enqueue (item)
               (let ((cell (list item)))
                 (if (null worklist)
                     (setf worklist cell)
                     (setf (cdr tail) cell))
                 (setf tail cell)))
             (check-cst (cst)
               (unless (gethash cst seen)
                 (setf (gethash cst seen) t)
                 (if (typep cst 'eclector.concrete-syntax-tree:wrapper-cst)
                     (check-cst (eclector.concrete-syntax-tree:target cst))
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
                               (enqueue cst-car)
                               (enqueue cst-cdr)))))))))
      (loop for cst = (pop worklist)
            until (null cst)
            do (check-cst cst)))))

(defun valid-cst-parse-result-p (client root-cst)
  (let* ((seen (make-hash-table :test #'eq))
         (tail (list root-cst))
         (worklist tail))
    (labels ((enqueue (item)
               (let ((cell (list item)))
                 (if (null worklist)
                     (setf worklist cell)
                     (setf (cdr tail) cell))
                 (setf tail cell)))
             (check-cst (cst)
               (unless (gethash cst seen)
                 (setf (gethash cst seen) t)
                 (typecase cst
                   (cst:atom-cst
                    (when (eclector.reader:labeled-object-state
                           client (cst:raw cst))
                      (return-from valid-cst-parse-result-p nil)))
                   (cst:cons-cst
                    (enqueue (cst:first cst))
                    (enqueue (cst:rest cst)))
                   (eclector.concrete-syntax-tree:wrapper-cst
                    (check-cst (eclector.concrete-syntax-tree:target cst)))))))
      (loop for cst = (pop worklist)
            until (null cst)
            do (check-cst cst))
      t)))
