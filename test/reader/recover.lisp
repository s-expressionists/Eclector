(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

(test recover/smoke
  "Test recovering from various syntax errors."

  (mapc (alexandria:named-lambda one-case (input-and-expected)
          (destructuring-bind (input expected-conditions expected-value
                               &optional (expected-position (length input)))
              input-and-expected
            (let ((remaining-conditions expected-conditions))
              (flet ((do-it ()
                       (handler-bind
                           ((error
                              (lambda (condition)
                                (let ((expected-condition (pop remaining-conditions)))
                                  (is (typep condition expected-condition)
                                      "For input ~S, expected a ~
                                       condition of type ~S but got ~
                                       ~S."
                                      input expected-condition condition))
                                (let ((restart (find-restart 'eclector.reader:recover)))
                                  (is-true (typep restart 'restart)
                                           "For input ~S expected a RECOVER restart."
                                           input)
                                  (unless restart
                                    (return-from one-case))
                                  (is (not (string= "" (princ-to-string restart)))
                                      "For input ~S expected restart to print properly."
                                      input)
                                  (invoke-restart restart)))))
                         (with-input-from-string (stream input)
                           (values (let ((eclector.reader::*backquote-depth* 1))
                                     (eclector.reader:read stream nil))
                                   (file-position stream))))))
                ;; Check expected value and position.
                (multiple-value-bind (value position) (do-it)
                  (is (relaxed-equalp expected-value value)
                      "For input ~S, expected return value ~S but got ~
                       ~S."  input expected-value value)
                  (is (equalp expected-position position)
                      "For input ~S, expected position ~S but got ~S."
                      input expected-position position))
                ;; All signaled conditions were as expected. Make sure
                ;; all expected conditions were signaled.
                (is (null remaining-conditions)
                    "For input ~S, expected condition~P ~S but those ~
                     were not signaled."
                    input
                    (length remaining-conditions) remaining-conditions)))))

        `(;; Recover from invalid syntax in symbols.
          (,(format nil ":foo~C" #\Backspace) (eclector.reader:invalid-constituent-character)                :foo_)
          (":fo\\"                            (eclector.reader:unterminated-single-escape-in-symbol)         :fo)
          (":fo|o"                            (eclector.reader:unterminated-multiple-escape-in-symbol)       :fo|o|)
          ("foo:"                             (eclector.reader:symbol-name-must-not-end-with-package-marker) foo|:|)
          (":foo:bar"                         (eclector.reader:two-package-markers-must-be-adjacent)         :foo|:|bar)
          ("::foo"                            (eclector.reader:two-package-markers-must-not-be-first)        :foo)
          ("eclector.reader.test:::foo"       (eclector.reader:symbol-can-have-at-most-two-package-markers)  |:|foo)

          ;; Recover from invalid number tokens.
          ("3/0" (eclector.reader:zero-denominator) 3)

          ;; Single quote
          ("'"         (eclector.reader:end-of-input-after-quote) 'nil)
          ("(')"       (eclector.reader:object-must-follow-quote) ('nil))

          ;; Double quote
          ("\""    (eclector.reader:unterminated-string)                  "")
          ("\"ab"  (eclector.reader:unterminated-string)                  "ab")
          ("\"a\\" (eclector.reader:unterminated-single-escape-in-string
                    eclector.reader:unterminated-string)                  "a")

          ;; Recover from quasiquotation-related errors.
          ("`"         (eclector.reader:end-of-input-after-backquote)           (eclector.reader:quasiquote nil))
          ("(`)"       (eclector.reader:object-must-follow-backquote)           ((eclector.reader:quasiquote nil)))

          ("`(1 ,)"    (eclector.reader:object-must-follow-unquote)             (eclector.reader:quasiquote
                                                                                 (1 (eclector.reader:unquote nil))))
          ("`,"        (eclector.reader:end-of-input-after-unquote)             (eclector.reader:quasiquote
                                                                                 (eclector.reader:unquote nil)))

          ("#C(,1 2)"  (eclector.reader:unquote-in-invalid-context)             #C(1 2))
          ("#C(`,1 2)" (eclector.reader:backquote-in-invalid-context
                        eclector.reader:unquote-not-inside-backquote)
                                                                                #C(1 2))

          ;; Recover from list-related errors
          ("("         (eclector.reader:unterminated-list)                      ())
          ("(1 2"      (eclector.reader:unterminated-list)                      (1 2))
          ("(1 ."      (eclector.reader:end-of-input-after-consing-dot
                        eclector.reader:unterminated-list)
                                                                                (1))
          ("(1 .)"     (eclector.reader:object-must-follow-consing-dot)         (1))
          ("(1 . 2 3)" (eclector.reader:multiple-objects-following-consing-dot) (1 . 2))
          (")(1)"      (eclector.reader:invalid-context-for-right-parenthesis)  (1))

          ("#("        (eclector.reader:unterminated-vector)                    #())
          ("#(1 2"     (eclector.reader:unterminated-vector)                    #(1 2))

          ;; Recover from errors in READ-RATIONAL.
          ("#b"    (eclector.reader:end-of-input-before-digit) 1)
          ("#b)"   (eclector.reader:digit-expected)            #b0   2)
          ("#b121" (eclector.reader:digit-expected)            #b111)
          ("#b1/"  (eclector.reader:end-of-input-before-digit) #b1/1)
          ("#b1/)" (eclector.reader:digit-expected)            #b1   4)
          ("#b1/0" (eclector.reader:zero-denominator)          #b1/1)

          ;; Recover from block-comment-related errors
          ("#|"        (eclector.reader:unterminated-block-comment)             nil)
          ("#|foo"     (eclector.reader:unterminated-block-comment)             nil)

          ;; Recover from errors related to uninterned symbols
          ("#::foo"    (eclector.reader:uninterned-symbol-must-not-contain-package-marker) #:|:|foo)
          ("#:foo:"    (eclector.reader:uninterned-symbol-must-not-contain-package-marker) #:foo|:|)
          ("#:fo\\"    (eclector.reader:unterminated-single-escape-in-symbol)              #:fo)
          ("#:fo|o"    (eclector.reader:unterminated-multiple-escape-in-symbol)            #:fo|o|)

          ("#"         (eclector.reader:unterminated-dispatch-macro)            nil)

          ;; Multiple subsequent recoveries needed.
          ("(1 (2"     (eclector.reader:unterminated-list
                        eclector.reader:unterminated-list)
                                                                                (1 (2)))
          ("(1 \"a"    (eclector.reader:unterminated-string
                        eclector.reader:unterminated-list)
                                                                                (1 "a")))))
