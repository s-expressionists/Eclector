(cl:in-package #:eclector.concrete-syntax-tree)

(defclass cst-client (eclector.parse-result:parse-result-client)
  ())

(defvar *cst-client* (make-instance 'cst-client))

;;; This method is responsible for constructing CST results from
;;; ordinary s-expression results.  It can do this by itself for a
;;; number of simple cases and it calls
;;; `concrete-syntax-tree:reconstruct' for difficult cases.
(defmethod eclector.parse-result:make-expression-result
    ((client cst-client) expression children source)
  (cond ((atom expression)
         (make-instance 'cst:atom-cst :raw expression :source source))
        ;; List structure with corresponding elements.
        ((and (eql (ignore-errors (list-length expression))
                   (length children))
              (every (lambda (sub-expression child)
                       (eql sub-expression (cst:raw child)))
                     expression children))
         (loop for expression in (loop with reversed = '()
                                       for sub-expression on expression
                                       do (push sub-expression reversed)
                                       finally (return reversed))
               for child in (reverse children)
               for previous = (make-instance 'cst:atom-cst :raw nil) then node
               for node = (make-instance 'cst:cons-cst :raw expression
                                                       :first child
                                                       :rest previous)
               finally (return (reinitialize-instance node :source source))))
        ;; Structure mismatch, try heuristic reconstruction.
        (t
         ;; We don't use
         ;;
         ;;   (cst:reconstruct client expression children)
         ;;
         ;; because we want SOURCE for the outer `cons-cst' but not
         ;; any of its children.
         (destructuring-bind (car . cdr) expression
           (make-instance 'cst:cons-cst
                          :raw expression
                          :first (cst:reconstruct client car children)
                          :rest (cst:reconstruct client cdr children)
                          :source source)))))
