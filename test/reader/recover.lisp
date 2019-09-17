(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

(test recover/smoke
  "Test recovering from various syntax errors."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected-conditions expected-value
                               &optional (expected-position (length input)))
              input-and-expected
            (let ((remaining-conditions expected-conditions))
              (flet ((do-it ()
                       (handler-bind
                           ((error
                              (lambda (condition)
                                (is (typep condition (pop remaining-conditions)))
                                (let ((restart (find-restart 'eclector.reader:recover)))
                                  (is (typep restart 'restart))
                                  (is (not (string= "" (princ-to-string restart))))
                                  (invoke-restart restart)))))
                         (with-input-from-string (stream input)
                           (values (let ((eclector.reader::*backquote-depth* 1))
                                     (eclector.reader:read stream nil))
                                   (file-position stream))))))
                (multiple-value-bind (value position) (do-it)
                  (is (equalp expected-value value))
                  (is (equalp expected-position position)))
                (is (null remaining-conditions))))))

        '(("("         (eclector.reader:unterminated-list)                      ())
          ("(1 2"      (eclector.reader:unterminated-list)                      (1 2))
          ("(1 ."      (eclector.reader:unterminated-list)                      (1))
          ("(1 .)"     (eclector.reader:object-must-follow-consing-dot)         (1))
          ("(1 . 2 3)" (eclector.reader:multiple-objects-following-consing-dot) (1 . 2))
          (")(1)"      (eclector.reader:invalid-context-for-right-parenthesis)  (1))

          ("#("        (eclector.reader:unterminated-vector)                    #())
          ("#(1 2"     (eclector.reader:unterminated-vector)                    #(1 2))

          ("\""        (eclector.reader:unterminated-string)                    "")
          ("\"ab"      (eclector.reader:unterminated-string)                    "ab")

          ("#|"        (eclector.reader:unterminated-block-comment)             nil)
          ("#|foo"     (eclector.reader:unterminated-block-comment)             nil)

          ("#"         (eclector.reader:unterminated-dispatch-macro)            nil)

          ("::foo"     (eclector.reader:two-package-markers-must-not-be-first)  :foo)

          ;; Recover from forbidden quasiquotation.
          ("#C(,1 2)"  (eclector.reader:unquote-in-invalid-context)             #C(1 2))
          ("#C(`,1 2)" (eclector.reader:backquote-in-invalid-context
                        eclector.reader:unquote-not-inside-backquote)
                                                                                #C(1 2))

          ;; Multiple subsequent recoveries needed.
          ("(1 (2"     (eclector.reader:unterminated-list
                        eclector.reader:unterminated-list)
                                                                                (1 (2)))
          ("(1 \"a"    (eclector.reader:unterminated-string
                        eclector.reader:unterminated-list)
                                                                                (1 "a")))))
