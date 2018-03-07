(cl:in-package #:eclector.reader)

(defgeneric read-common (client input-stream eof-error-p eof-value))

(defgeneric read-token (input-stream eof-error-p eof-value))

(defgeneric note-skipped-input (client input-stream)
  (:method ((client t) (input-stream t))
    (declare (ignore client input-stream))))

(defgeneric interpret-token (token token-escapes input-stream))

(defgeneric interpret-symbol (token
                              position-package-marker-1
                              position-package-marker-2
                              input-stream))

(defgeneric call-reader-macro (function input-stream char))

(defgeneric fixup (object seen-objects mapping))
