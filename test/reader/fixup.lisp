(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.fixup
  :in :eclector.reader)

(defclass mock-object ()
  ((%a :initarg :a :reader a)
   (%b :initarg :b :reader b)))

(test fixup/smoke
  "Smoke test for the FIXUP generic function."

  (map nil (lambda (object-mapping-expected)
             (destructuring-bind (object mapping expected)
                 object-mapping-expected
               (let ((seen (make-hash-table :test #'eq))
                     (mapping (alexandria:alist-hash-table mapping)))
                 (eclector.reader:fixup t object seen mapping)
                 (typecase object
                   (mock-object
                    (let ((slot-values (list (if (slot-boundp object '%a)
                                                 (a object)
                                                 'unbound)
                                             (b object))))
                      (is (equalp expected slot-values))))
                   (hash-table
                    (let ((as-alist (sort (alexandria:hash-table-alist object) #'<
                                          :key (lambda (cell)
                                                 (destructuring-bind (car . cdr) cell
                                                   (if (realp car) car cdr))))))
                      (is (equalp expected as-alist))))
                   (t (is (equalp expected object)))))))

       (list ;; vector
             (let* ((a (gensym))
                    (marker (list t)))
               (list (vector a marker)
                     (list (cons marker a))
                     (vector a a)))
             ;; standard-object
             (let* ((a (gensym))
                    (marker (list t)))
               (list (make-instance 'mock-object :a a :b marker)
                     (list (cons marker a))
                     (list a a)))
             ;; standard-object with unbound slot
             (let* ((a (gensym))
                    (marker (list t)))
               (list (make-instance 'mock-object :b marker)
                     (list (cons marker a))
                     (list 'unbound a)))
             ;; hash-table
             (let* ((a (gensym))
                    (b (gensym))
                    (marker1 (list t))
                    (marker2 (list t)))
               (list (alexandria:alist-hash-table
                      (list (cons a 1) (cons 2 a) (cons 3 marker1)
                            (cons b 4) (cons 5 marker2) (cons 6 b)))
                     (list (cons marker1 a) (cons marker2 b))
                     (list (cons a 1) (cons 2 a) (cons 3 a)
                           (cons b 4) (cons 5 b) (cons 6 b)))))))
