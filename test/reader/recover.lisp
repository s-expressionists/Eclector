(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

(test recover/smoke
  "Test recovering from various syntax errors."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected-value expected-recover-count)
              input-and-expected
            (let ((recover-count 0))
              (flet ((do-it ()
                       (handler-bind
                           ((error
                              (lambda (condition)
                                (declare (ignore condition))
                                (let ((restart (find-restart 'eclector.reader:recover)))
                                  (is (typep restart 'restart))
                                  (is (not (string= "" (princ-to-string restart))))
                                  (incf recover-count)
                                  (invoke-restart restart)))))
                         (with-input-from-string (stream input)
                           (eclector.reader:read stream nil)))))
                (is (equalp expected-value (do-it)))
                (is (eql expected-recover-count recover-count))))))

        '(("("         ()      1)
          ("(1 2"      (1 2)   1)
          ("(1 ."      (1)     1)
          ("(1 . 2 3)" (1 . 2)     1)

          ("#("        #()     1)
          ("#(1 2"     #(1 2)  1)

          ("\""        ""      1)
          ("\"ab"      "ab"    1)

          ("#|"        nil     1)
          ("#|foo"     nil     1)

          ("#"         nil     1)

          ;; Multiple subsequent recoveries needed.
          ("(1 (2"     (1 (2)) 2)
          ("(1 \"a"    (1 "a") 2))))
