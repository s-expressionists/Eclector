(cl:in-package #:eclector)

(with-input-from-string (stream "(progn 1 2 2 3)")
 (read stream))

(utilities.print-tree:print-tree
 *standard-output*
 (with-input-from-string (stream "(progn #|foo|# #.(list 1 2)
                                  ; bar
                                   1 (+ 2 3) 4)")
   (cst-read stream))
 (utilities.print-tree:make-node-printer
  (lambda (stream level node)
    (typecase node
      (cst:atom-cst
       (princ (cst:raw node) stream))
      (t
       (princ (class-name (class-of node)) stream)))
    (format stream "~@[  @~D-~D~]"
            (car (cst:source node))
            (cdr (cst:source node)))
    nil)
  nil
  (lambda (node)
    (typecase node
      (cst:cons-cst
       (list (cst:first node) (cst:rest node)))))))
