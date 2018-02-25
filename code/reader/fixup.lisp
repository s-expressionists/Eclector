(cl:in-package #:eclector.reader)

(defgeneric fixup (object seen-objects mapping))

(defmethod fixup :around (object seen-objects mapping)
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (call-next-method)))

(defmethod fixup (object seen-objects mapping)
  (declare (ignore seen-objects mapping))
  nil)

(macrolet ((fixup-place (place)
             `(multiple-value-bind (value found-p)
                  (gethash ,place mapping)
                (if found-p
                    (setf ,place value)
                    (fixup ,place seen-objects mapping)))))

  (defmethod fixup ((object cons) seen-objects mapping)
    (fixup-place (car object))
    (fixup-place (cdr object)))

  (defmethod fixup ((object array) seen-objects mapping)
    (loop for i from 0 below (array-total-size object)
       do (fixup-place (row-major-aref object i))))

  (defmethod fixup ((object standard-object) seen-objects mapping)
    (loop for slot-definition in (closer-mop:class-slots (class-of object))
       for name = (closer-mop:slot-definition-name slot-definition)
       do (fixup-place (slot-value object name))))

  (defmethod fixup ((object hash-table) seen-objects mapping)
    (maphash (lambda (key val)
               (multiple-value-bind (value found-p)
                   (gethash val mapping)
                 (if found-p
                     (setf (gethash key object) value)
                     (fixup value seen-objects mapping))))
             object)))
