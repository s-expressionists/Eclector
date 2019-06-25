(cl:defpackage #:eclector.parse-result.test
  (:use
   #:common-lisp
   #:alexandria
   #:fiveam)

  (:import-from #:eclector.test
   #:error-case)

  (:export
   #:run-tests))

(cl:in-package #:eclector.parse-result.test)

(def-suite :eclector.parse-result
  :in :eclector)

(defun run-tests ()
  (run! :eclector.parse-result))

(defun call-with-input-stream (thunk input &key input-length)
  (let* ((input (format nil input))
         (input-length (or input-length (length input))))
    (with-input-from-string (stream input)
      (multiple-value-call #'values
        (funcall thunk stream input-length)
        (file-position stream)))))

(defun do-read (input eof-error-p eof-value
                &key
                (read-function 'eclector.parse-result:read)
                (client 'simple-result-client))
  (let ((input (format nil input)))
    (with-input-from-string (stream input)
      (values (funcall read-function
                       (apply 'make-instance (ensure-list client))
                       stream eof-error-p eof-value)
              (file-position stream)))))
