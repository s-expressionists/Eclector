(cl:in-package #:eclector.reader)

(defgeneric read-common (client input-stream eof-error-p eof-value))

(defgeneric read-token (client input-stream eof-error-p eof-value))

(defgeneric note-skipped-input (client input-stream reason)
  (:method ((client t) (input-stream t) (reason t))
    (declare (ignore client input-stream reason))))

(defgeneric interpret-token (token token-escapes input-stream))

(defgeneric interpret-symbol (token
                              position-package-marker-1
                              position-package-marker-2
                              input-stream))

(defgeneric call-reader-macro (client input-stream char readtable))

(defgeneric fixup (object seen-objects mapping))
