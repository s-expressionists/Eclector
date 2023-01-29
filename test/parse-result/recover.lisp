(cl:in-package #:eclector.parse-result.test)

(def-suite* :eclector.parse-result.recover
  :in :eclector.parse-result)

(defun read-with-simple-result-client (stream)
  (let ((client (make-instance 'simple-result-client)))
    (raw (eclector.parse-result:read client stream))))

(test recover/labeled-objects
  "Test recovering from syntax errors related to labeled objects."
  (mapc (alexandria:rcurry #'eclector.reader.test::do-recover-test-case
                           #'read-with-simple-result-client)
        '(("#1=#1#" (eclector.reader:sharpsign-equals-only-refers-to-self)
                    nil)
          ("(#1="   (eclector.reader:end-of-input-after-sharpsign-equals
                     eclector.reader:unterminated-list)
                    (nil)))))
