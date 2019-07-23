(cl:in-package #:eclector.base)

(macrolet
    ((define-reporter (((condition-var condition-specializer) stream-var)
                       &body body)
       `(defmethod acclimation:report-condition
            ((,condition-var ,condition-specializer)
             ,stream-var
             (language acclimation:english))
          ,@body)))

  (define-reporter ((condition end-of-file) stream)
    (format stream "Unexpected end of input.")))
