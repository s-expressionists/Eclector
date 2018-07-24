(cl:in-package #:eclector.reader)

(defgeneric read-common (client input-stream eof-error-p eof-value))

(defgeneric read-token (client input-stream eof-error-p eof-value))

(defgeneric note-skipped-input (client input-stream reason)
  (:method ((client t) (input-stream t) (reason t))
    (declare (ignore client input-stream reason))))

(defgeneric interpret-token (client input-stream token escape-ranges))

(defgeneric interpret-symbol-token (client input-stream
                                    token
                                    position-package-marker-1
                                    position-package-marker-2))

(defgeneric interpret-symbol (client input-stream
                              package-name symbol-name internp))

(defgeneric call-reader-macro (client input-stream char readtable))

(defgeneric evaluate-expression (client expression)
  (:method ((client t) (expression t))
    (declare (ignore client))
    (eval expression)))

(defgeneric check-feature-expression (client feature-expression)
  (:method ((client t) (feature-expression t))
    (declare (ignore client))
    (check-standard-feature-expression feature-expression)))

(defgeneric evaluate-feature-expression (client feature-expression)
  (:method ((client t) (feature-expression t))
    (evaluate-standard-feature-expression
     feature-expression
     :check (alexandria:curry #'check-feature-expression client)
     :recurse (alexandria:curry #'evaluate-feature-expression client))))

(defgeneric fixup (object seen-objects mapping))
