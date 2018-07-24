(cl:in-package #:eclector.reader.test)

(in-suite :eclector.reader)

(test read/smoke
  "Smoke test for the READ function."

  ;; This test focuses on interactions between different parts of the
  ;; reader since the individual parts in isolation are handled by
  ;; more specific tests.
  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected) input-and-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.reader:read stream)
                               (file-position stream)))))
              (case expected
                (eclector.reader:invalid-context-for-backquote
                 (signals eclector.reader:invalid-context-for-backquote
                   (do-it)))
                (eclector.reader:comma-not-inside-backquote
                 (signals eclector.reader:comma-not-inside-backquote
                   (do-it)))
                (eclector.reader:object-must-follow-comma
                 (signals eclector.reader:object-must-follow-comma
                   (do-it)))
                (eclector.reader:unknown-macro-sub-character
                 (signals eclector.reader:unknown-macro-sub-character
                   (do-it)))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected       result))
                   (is (eql   (length input) position))))))))

        '(("(cons 1 2)"                 (cons 1 2))

          ("#+(or) `1 2"                2)
          ("#+(or) #.(error \"foo\") 2" 2)

          ;; Some context-sensitive cases.
          ("#C(1 `,2)"                  eclector.reader:invalid-context-for-backquote)
          ("#+`,common-lisp 1"          eclector.reader:invalid-context-for-backquote)
          (",foo"                       eclector.reader:comma-not-inside-backquote)
          (",@foo"                      eclector.reader:comma-not-inside-backquote)
          ("`(,)"                       eclector.reader:object-must-follow-comma)
          ("`(,@)"                      eclector.reader:object-must-follow-comma)
          ("`(,.)"                      eclector.reader:object-must-follow-comma)
          ("#1=`(,2)"                   (eclector.reader:quasiquote ((eclector.reader:unquote 2))))

          ;; Interaction between *READ-SUPPRESS* and reader macros.
          ("#+(or) #|skipme|# 1 2"      2)
          ("#+(or) ; skipme
            1 2"                        2)

          ;; Unknown macro sub character.
          ("#!"                         eclector.reader:unknown-macro-sub-character))))
