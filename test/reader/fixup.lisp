(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.fixup
  :in :eclector.reader)

;;; Count fixup calls
;;;
;;; Avoiding unnecessary traversal of read objects and unnecessary
;;; `fixup' calls is an important aspect of the labeled objects
;;; sub-system. The following client helps ensuring that no
;;; unnecessary `fixup' calls are made.

(defclass call-counting-client ()
  ((%fixup-count :accessor fixup-count
                 :initform 0)))

(defmethod eclector.reader:fixup :after ((client call-counting-client)
                                         object
                                         seen-objects
                                         mapping)
  (declare (ignore object seen-objects mapping))
  (incf (fixup-count client)))

;;; Tests

(defclass mock-object ()
  ((%a :initarg :a :reader a)
   (%b :initarg :b :reader b)))

(test fixup/smoke
  "Smoke test for the FIXUP generic function."
  (mapc (lambda (object-mapping-expected)
          (destructuring-bind (object mapping expected fixup-count)
              object-mapping-expected
            (let ((client (make-instance 'call-counting-client))
                  (seen (make-hash-table :test #'eq))
                  (mapping (alexandria:alist-hash-table mapping)))
              (eclector.reader:fixup client object seen mapping)
              (typecase object
                (mock-object
                 (let ((slot-values (list (if (slot-boundp object '%a)
                                              (a object)
                                              'unbound)
                                          (b object))))
                   (is (equalp expected slot-values))))
                (hash-table
                 (let ((alist (alexandria:hash-table-alist object)))
                   (is (alexandria:set-equal expected alist :test #'equal)
                       "~@<Expected hash table entries ~S but got ~S. ~
                        Mismatches: ~S and ~S~@:>"
                       expected alist
                       (set-difference expected alist :test #'equal)
                       (set-difference alist expected :test #'equal))))
                (t (is (equalp expected object))))
              (is (= fixup-count (fixup-count client))
                  "~@<For object, ~S expected ~A to be called ~D time~:P, ~
                   but it was called ~D time~:P~@:>"
                  object 'eclector.reader:fixup
                  fixup-count (fixup-count client)))))
        (list ;; cons
              (let* ((a (gensym))
                     (marker (list t)))
                (list (list 1 marker a (cons 2 marker))
                      (list (cons marker a))
                      (list 1 a      a (cons 2 a))
                      9))
              ;; vector
              (let* ((a (gensym))
                     (marker (list t)))
                (list (vector a marker)
                      (list (cons marker a))
                      (vector a a)
                      2))
              ;; Specialized arrays (smoke test since nothing has to be fixed up)
              (list "foo" '() "foo" 1)
              #.(if (subtypep (upgraded-array-element-type '(unsigned-byte 8))
                              'number)
                    '(list (make-array 2 :element-type     '(unsigned-byte 8)
                                         :initial-contents '(1 2))
                      '()
                      (make-array 2 :element-type     '(unsigned-byte 8)
                                    :initial-contents '(1 2))
                      1)
                    '(list nil '() nil 1))
              ;; standard-object
              (let* ((a (gensym))
                     (marker (list t)))
                (list (make-instance 'mock-object :a a :b marker)
                      (list (cons marker a))
                      (list a a)
                      2))
              ;; standard-object with unbound slot
              (let* ((a (gensym))
                     (marker (list t)))
                (list (make-instance 'mock-object :b marker)
                      (list (cons marker a))
                      (list 'unbound a)
                      1))
              ;; hash-table
              (let* ((a (gensym))
                     (b (gensym))
                     (c (gensym))
                     (marker1 (list t))
                     (marker2 (list t))
                     (marker3 (list t)))
                (list (alexandria:alist-hash-table
                       (list (cons a 1) (cons 2 a) (cons 3 marker1)
                             (cons b 4) (cons 5 marker2) (cons 6 b)
                             (cons 7 (cons 8 marker3))))
                      (list (cons marker1 a) (cons marker2 b) (cons marker3 c))
                      (list (cons a 1) (cons 2 a) (cons 3 a)
                            (cons b 4) (cons 5 b) (cons 6 b)
                            (cons 7 (cons 8 c)))
                      7)))))
