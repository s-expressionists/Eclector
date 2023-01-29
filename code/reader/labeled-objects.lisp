(cl:in-package #:eclector.reader)

(defmethod fixup-graph (client root-object mapping)
  (let ((seen (make-hash-table :test #'eq)))
    (fixup client root-object seen mapping)))

(defmethod fixup :around (client object seen-objects mapping)
  (declare (ignore client mapping))
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (call-next-method)))

(defmethod fixup (client object seen-objects mapping)
  (declare (ignore client object seen-objects mapping))
  nil)

(macrolet ((fixup-place (place)
             `(let ((current-value ,place))
                (multiple-value-bind (value found-p)
                    (gethash current-value mapping)
                  (if found-p
                      (setf ,place value)
                      (fixup client current-value seen-objects mapping))))))

  (defmethod fixup (client (object cons) seen-objects mapping)
    (fixup-place (car object))
    (fixup-place (cdr object)))

  (defmethod fixup (client (object array) seen-objects mapping)
    ;; Fix up array elements unless the array element type indicates
    ;; that no fix up is required.
    (let ((element-type (array-element-type object)))
      (when (or (eq element-type 't) ; fast path
                (not (subtypep element-type '(or character number symbol))))
        (loop for i from 0 below (array-total-size object)
              do (fixup-place (row-major-aref object i))))))

  (defmethod fixup (client (object standard-object) seen-objects mapping)
    (loop for slot-definition in (closer-mop:class-slots (class-of object))
          for name = (closer-mop:slot-definition-name slot-definition)
          when (slot-boundp object name)
            do (fixup-place (slot-value object name))))

  (defmethod fixup (client (object hash-table) seen-objects mapping)
    (let ((key-changes '()))
      (maphash (lambda (key value)
                 ;; If KEY has to be replaced, remove the entry for
                 ;; KEY and VALUE (which is the only entry that may be
                 ;; removed or changed according to the traversal
                 ;; rules) and store the information for adding the
                 ;; final key in KEY-CHANGES (since the traversal
                 ;; rules forbid adding new entries). Fix up VALUE by
                 ;; either fixing up the (gethash key object) place or
                 ;; fixing up the value part of the queued KEY-CHANGES
                 ;; entry.
                 (macrolet ((fixup-value (place value)
                              `(multiple-value-bind (final-value found-p)
                                   (gethash ,value mapping)
                                 (if found-p
                                     (setf ,place final-value)
                                     (fixup client ,value seen-objects mapping)))))
                   (multiple-value-bind (final-key found-p)
                       (gethash key mapping)
                     (cond (found-p
                            (remhash key object)
                            (let ((change (cons final-key value)))
                              (fixup-value (cdr change) value)
                              (push change key-changes)))
                           (t
                            (fixup client key seen-objects mapping)
                            (fixup-value (gethash key object) value))))))
               object)
      (loop for (final-key . final-value) in key-changes
            do (setf (gethash final-key object) final-value)))))
