(cl:in-package #:eclector.concrete-syntax-tree)

;;; Fixup

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object cst:atom-cst)
                                  seen-objects)
  (declare (ignore seen-objects)))

(defmethod eclector.reader:fixup ((client cst-client)
                                  (object cst:cons-cst)
                                  seen-objects)
  (macrolet ((fixup-place (place)
               ;; Determine the labeled object state of the raw value
               ;; of the CST stored in PLACE. For a finalized labeled
               ;; object, create a "reference" CST object (which may
               ;; just be the parse result for the circular object,
               ;; depending on the client) and replace the value of
               ;; PLACE with that reference CST object.
               `(let* ((current-value ,place)
                       (current-raw-value (cst:raw current-value)))
                  (eclector.reader:fixup-case (client current-raw-value)
                    (() ; not a labeled object
                     (eclector.reader:fixup client current-value seen-objects))
                    (() ; finalized labeled object
                     (let ((source (cst:source current-value)))
                       ;; CURRENT-RAW-VALUE is a finalized labeled
                       ;; object from which the (circular) final raw
                       ;; value and parse result can be obtained.
                       (setf ,place (eclector.parse-result:make-expression-result
                                     client
                                     eclector.parse-result:**reference**
                                     current-raw-value
                                     source))))))))
    (fixup-place (slot-value object 'cst::%first))
    (fixup-place (slot-value object 'cst::%rest))))
