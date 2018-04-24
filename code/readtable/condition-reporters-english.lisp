(cl:in-package #:eclector.readtable)

(macrolet ((define-reporter (((condition-var condition-specializer) stream-var)
                             &body body)
             `(defmethod acclimation:report-condition
                  ((,condition-var ,condition-specializer)
                   ,stream-var
                   (language acclimation:english))
                ,@body)))

  (define-reporter ((condition sub-char-must-not-be-a-decimal-digit) stream)
    (format stream "~:c cannot be defined as a dispatch macro ~
                    sub-character, as it is a decimal digit."
            (sub-char condition)))

  (define-reporter ((condition char-must-be-a-dispatching-character) stream)
    (format stream "~:c cannot have a dispatch macro set for it, as it ~
                    has not been defined as a dispatch macro~@
                    (as by ~a)"
            (disp-char condition) 'cl:make-dispatch-macro-character))

  (define-reporter ((condition unknown-macro-sub-character) stream)
    (format stream "~:c is not a sub-character of the dispatch macro ~
                    character ~:c."
            (sub-char condition) (disp-char condition))))
