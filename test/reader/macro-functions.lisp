(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.macro-functions
    :in :eclector.reader)

(test semicolon/smoke
  "Smoke test for the SEMICOLON reader macro function."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected &optional expected-position)
              input-expected
            (let* ((input (format nil input))
                   (expected-position (or expected-position
                                          (length input))))
              (multiple-value-bind (result position)
                  (with-input-from-string (stream input)
                    (values
                     (multiple-value-list
                      (eclector.reader::semicolon stream #\;))
                     (file-position stream)))
                (is (equal expected          result))
                (is (eql   expected-position position))))))
        '((""    ())
          ("~%"  () 0)
          (";"   ())
          (";~%" () 1))))

(test single-quote/smoke
  "Smoke test for the SINGLE-QUOTE reader macro function."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected
                               &optional (expected-position (length input)))
              input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.reader::single-quote stream #\')
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected          result))
                   (is (eql   expected-position position))))))))
        '((""  eclector.reader:end-of-input-after-quote)
          (")" eclector.reader:object-must-follow-quote)

          ("1" (quote 1)))))

(test double-quote/smoke
  "Smoke test for the DOUBLE-QUOTE reader macro function."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected &optional expected-position)
              input-expected
            (let* ((input (format nil input))
                   (expected-position (or expected-position
                                          (length input))))
              (flet ((do-it ()
                       (with-input-from-string (stream input)
                         (values (eclector.reader::double-quote stream #\")
                                 (file-position stream)))))
                (error-case expected
                  (error (do-it))
                  (t
                   (multiple-value-bind (result position) (do-it)
                     (is (equal expected          result))
                     (is (eql   expected-position position)))))))))
        '((""       eclector.reader:unterminated-string)
          ("\\"     eclector.reader:unterminated-single-escape-in-string)

          ("\""     "")

          ("a"      eclector.reader:unterminated-string)

          ("\\a"    eclector.reader:unterminated-string)
          ("\\a\""  "a")
          ("\\\\"   eclector.reader:unterminated-string)
          ("\\\\\"" "\\")
          ("\\\""   eclector.reader:unterminated-string)
          ("\\\"\"" "\""))))

(test backquote/smoke
  "Smoke test for the BACKQUOTE reader macro function."

  (mapc (lambda (input-forbidden-expected)
          (destructuring-bind (input quasiquote-forbidden expected
                               &optional (expected-position (length input)))
              input-forbidden-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((eclector.reader::*quasiquote-forbidden*
                                       (if quasiquote-forbidden
                                           'eclector.reader::sharpsign-a
                                           nil))
                                     (eclector.reader::*unquote-forbidden*
                                       'eclector.reader::sharpsign-a)
                                     (eclector.reader::*backquote-depth* 0))
                                 (eclector.reader::backquote stream #\`))
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected          result))
                   (is (eql   expected-position position))))))))
        '(;; Errors
          (""   nil eclector.reader:end-of-input-after-backquote)
          (")"  nil eclector.reader:object-must-follow-backquote)
          ("1"  t   eclector.reader:backquote-in-invalid-context)
          ;; Valid
          ("1"  nil (eclector.reader:quasiquote 1))
          (",1" nil (eclector.reader:quasiquote (eclector.reader:unquote 1))))))

(test comma/smoke
  "Smoke test for the COMMA reader macro function."

  (mapc (lambda (input-backquote-context-expected)
          (destructuring-bind (input unquote-forbidden backquote-depth
                               expected
                               &optional (expected-position (length input)))
              input-backquote-context-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((eclector.reader::*unquote-forbidden*
                                       (if unquote-forbidden
                                           'eclector.reader::sharpsign-a
                                           nil))
                                     (eclector.reader::*backquote-depth*
                                       backquote-depth))
                                 (eclector.reader::comma stream #\,))
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected          result))
                   (is (eql   expected-position position))))))))
        '(;; Errors
          (""       nil 1 eclector.reader:end-of-input-after-unquote)
          ("@"      nil 1 eclector.reader:end-of-input-after-unquote)
          ("."      nil 1 eclector.reader:end-of-input-after-unquote)
          (")"      nil 1 eclector.reader:object-must-follow-unquote)
          ("@)"     nil 1 eclector.reader:object-must-follow-unquote)
          (".)"     nil 1 eclector.reader:object-must-follow-unquote)
          ("1"      nil 0 eclector.reader:unquote-not-inside-backquote)
          ("1"      t   1 eclector.reader:unquote-in-invalid-context)
          (","      t   1 eclector.reader:unquote-in-invalid-context)
          ;; Valid
          ("1"      nil 1 (eclector.reader:unquote 1))
          ("@1"     nil 1 (eclector.reader:unquote-splicing 1))
          (".1"     nil 1 (eclector.reader:unquote-splicing 1)))))

(test left-parenthesis/smoke
  "Smoke test for the LEFT-PARENTHESIS reader macro function."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected
                               &optional (expected-position (length input)))
              input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.reader::left-parenthesis stream #\()
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)

                   (is (equal expected          result))
                   (is (eql   expected-position position))))))))
        '((""        eclector.reader:unterminated-list)

          ("."       eclector.reader:invalid-context-for-consing-dot)
          ("1 ."     eclector.reader:end-of-input-after-consing-dot)
          ("1 . )"   eclector.reader:object-must-follow-consing-dot)
          ("1 . 2 3" eclector.reader:multiple-objects-following-consing-dot)
          ("1 . ."   eclector.reader:invalid-context-for-consing-dot)

          (")"       ())
          ("1)"      (1))
          ("1 2)"    (1 2))

          ;; Trailing whitespace. Not consuming it seems to be
          ;; permitted irregardless of whether we were asked to
          ;; preserve whitespace.
          (") "    ()    1))))

(test right-parenthesis/smoke
  "Smoke test for the RIGHT-PARENTHESIS reader macro function."

  (signals-printable eclector.reader:invalid-context-for-right-parenthesis
    (with-input-from-string (stream ")")
      (eclector.reader::right-parenthesis stream #\)))))

(test sharpsign-single-quote/smoke
  "Smoke test for the SHARPSIGN-SINGLE-QUOTE reader macro function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind (input parameter read-suppress expected
                               &optional (expected-position (length input)))
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress)
                             (eclector.reader::*backquote-depth* 1))
                         (values (eclector.reader::sharpsign-single-quote
                                  stream #\' parameter)
                                 (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected          result))
                   (is (eql   expected-position position))))))))
        '(;; Errors
          (""                nil nil eclector.reader:end-of-input-after-sharpsign-single-quote)
          (")"               nil nil eclector.reader:object-must-follow-sharpsign-single-quote)
          ("5"               nil nil (function 5))
          ("X"               1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          (",foo"            nil nil eclector.reader:unquote-in-invalid-context)
          ("(lambda () ,1)"  nil nil eclector.reader:unquote-in-invalid-context)
          ;; Valid
          ("X"               nil nil (function X))
          ("CL-USER::X"      nil nil (function cl-user::x))
          ("(lambda ())"     nil nil (function (lambda ())))
          ("(lambda () `,1)" nil nil (function (lambda ()
                                                 (eclector.reader:quasiquote
                                                  (eclector.reader:unquote 1)))))
          ("X "              nil nil (function X) 1)
          ;; With *READ-SUPPRESS* bound to T
          ("X"               nil t   nil)
          ("5"               nil t   nil)
          ("X"               1   t   nil))))

(test sharpsign-left-parenthesis/smoke
  "Smoke test for the SHARPSIGN-LEFT-PARENTHESIS reader macro function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind (input parameter read-suppress expected
                               &optional (expected-position (length input)))
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress))
                         (values (eclector.reader::sharpsign-left-parenthesis
                                  stream #\( parameter)
                                 (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)

                   (is (equalp expected          result))
                   (is (eql    expected-position position))))))))
        '(;; Errors
          (""       nil nil eclector.reader:unterminated-vector)
          ("1"      nil nil eclector.reader:unterminated-vector)
          (")"      2   nil eclector.reader:no-elements-found)
          ("1 1 1)" 2   nil eclector.reader:too-many-elements)
          ;; Valid
          (")"      nil nil #())
          (") "     nil nil #()    1)
          ("))"     nil nil #()    1)
          ("1 1)"   nil nil #(1 1))
          ("1 1) "  nil nil #(1 1) 4)
          (")"      0   nil #0())
          ("1)"     1   nil #1(1))
          ("1)"     2   nil #2(1))
          ;; With *READ-SUPPRESS* bound to T
          (")"      nil t   nil)
          ("1)"     nil t   nil)
          ("1)"     2   t   nil)
          ("1 1)"   2   t   nil)
          ("1 2 3)" 2   t   nil))))

(test sharpsign-dot/smoke
  "Smoke test for the SHARPSIGN-DOT reader macro function."

  (mapc (lambda (input-parameters-expected)
          (destructuring-bind
              (input parameter read-suppress read-eval expected)
              input-parameters-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((*read-suppress* read-suppress)
                                     (*read-eval* read-eval))
                                 (eclector.reader::sharpsign-dot
                                  stream #\. parameter))
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected       result))
                   (is (eql   (length input) position))))))))
        '(;; Error cases
          (""                nil nil t   eclector.reader:end-of-input-after-sharpsign-dot)
          (")"               nil nil t   eclector.reader:object-must-follow-sharpsign-dot)
          ("1"               1   nil t   eclector.reader:numeric-parameter-supplied-but-ignored)
          ("1"               nil nil nil eclector.reader:read-time-evaluation-inhibited)
          ("(error \"foo\")" nil nil t   eclector.reader:read-time-evaluation-error)
          ;; Good inputs
          ("1"               nil nil t   1)
          ("(+ 1 2)"         nil nil t   3)

          ("`1"              nil nil t   1)
          ;; With *READ-SUPPRESS* bound to T
          ("(error \"foo\")" nil t   t   nil)
          ("1"               1   t   t   nil))))

(test sharpsign-backslash/smoke
  "Smoke test for the SHARPSIGN-BACKSLASH reader macro function."

  (mapc (lambda (input-parameters-expected)
          (destructuring-bind (input parameter read-suppress expected
                               &optional (expected-position (length input)))
              input-parameters-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((*read-suppress* read-suppress))
                                 (eclector.reader::sharpsign-backslash
                                  stream #\\ parameter))
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected          result))
                   (is (eql   expected-position position))))))))
        '(;; Error cases
          (""                  nil nil eclector.reader:end-of-input-after-backslash)
          ("x\\"               nil nil eclector.reader:unterminated-single-escape-in-character-name)
          ("x|"                nil nil eclector.reader:unterminated-multiple-escape-in-character-name)
          ("\\no-such-char"    nil nil eclector.reader:unknown-character-name)
          ("|no-such-char"     nil nil eclector.reader:unknown-character-name)
          ("no-such-char"      nil nil eclector.reader:unknown-character-name)
          ("no-such:char"      nil nil eclector.reader:unknown-character-name)
          ("no-such::char"     nil nil eclector.reader:unknown-character-name)
          ("no-such:::char"    nil nil eclector.reader:unknown-character-name)
          ("a"                 1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ;; Single-character case
          ("a"                 nil nil #\a)
          ("A"                 nil nil #\A)
          ("("                 nil nil #\()
          ("\\"                nil nil #\\)
          ("|"                 nil nil #\|)
          ("a "                nil nil #\a)
          ("a("                nil nil #\a 1)
          ;; Multi-character case
          ("space"             nil nil #\Space)
          ("Space"             nil nil #\Space)
          ("SPACE"             nil nil #\Space)
          ("Ret\\urn"          nil nil #\Return)
          ("Re|tur|n"          nil nil #\Return)
          ("Re|t\\ur|n"        nil nil #\Return)
          ;; With *READ-SUPPRESS* bound to T
          (""                  nil t   eclector.reader:end-of-input-after-backslash)
          ("x\\"               nil t   eclector.reader:unterminated-single-escape-in-character-name)
          ("x|"                nil t   eclector.reader:unterminated-multiple-escape-in-character-name)
          ("no-such-char"      nil t   nil)
          ("1"                 1   t   nil))))

(test read-rational/smoke
  "Smoke test for the READ-RATIONAL reader macro function."

  (mapc (lambda (input-base-preserve-expected)
          (destructuring-bind
              (input base eclector.reader:*preserve-whitespace* expected
               &optional (expected-position (length input)))
              input-base-preserve-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.reader::read-rational stream base)
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (eql expected          value))
                   (is (eql expected-position position))))))))
        '(;; Error cases
          (""       10 nil eclector.reader:end-of-input-before-digit)
          (" "      10 nil eclector.reader:digit-expected)
          ("x"      10 nil eclector.reader:digit-expected)
          ("|"      10 nil eclector.reader:digit-expected)
          ("- "     10 nil eclector.reader:digit-expected)
          ("-x"     10 nil eclector.reader:digit-expected)
          ("1x"     10 nil eclector.reader:digit-expected)
          ("1#"     10 t   eclector.reader:digit-expected)
          ("1/"     10 nil eclector.reader:end-of-input-before-digit)
          ("1/ "    10 nil eclector.reader:digit-expected)
          ("1/x"    10 nil eclector.reader:digit-expected)
          ("1/1x"   10 nil eclector.reader:digit-expected)
          ("1/1#"   10 nil eclector.reader:digit-expected)
          ("1/0"    10 nil eclector.reader:zero-denominator)
          ;; Good inputs
          ("1"      10 nil 1)
          ("-1"     10 nil -1)
          ("1 "     10 nil 1)
          ("-1 "    10 nil -1)
          ("1 "     10 t   1     1)
          ("-1 "    10 t   -1    2)
          ("12"     10 nil 12)
          ("-12"    10 nil -12)
          ("12 "    10 nil 12)
          ("-12 "   10 nil -12)
          ("12 "    10 t   12    2)
          ("-12 "   10 t   -12   3)
          ("12("    10 nil 12    2)
          ("-12("   10 nil -12   3)
          ("1/2"    10 nil 1/2)
          ("-1/2"   10 nil -1/2)
          ("1/2 "   10 nil 1/2)
          ("-1/2 "  10 nil -1/2)
          ("1/2 "   10 t   1/2   3)
          ("-1/2 "  10 t   -1/2  4)
          ("1/2("   10 nil 1/2   3)
          ("-1/2("  10 nil -1/2  4)
          ("1/23"   10 nil 1/23)
          ("-1/23"  10 nil -1/23)
          ("1/23 "  10 nil 1/23)
          ("-1/23 " 10 nil -1/23)
          ("1/23 "  10 t   1/23  4)
          ("-1/23 " 10 t   -1/23 5)
          ("1 2"    10 nil 1     2)
          ("1 2 "   10 t   1     1)
          ;; Base
          ("1"      10 nil 1)
          ("10"     10 nil 10)
          ("a"      10 nil eclector.reader:digit-expected)
          ("z"      10 nil eclector.reader:digit-expected)

          ("1"      16 nil 1)
          ("10"     16 nil 16)
          ("a"      16 nil 10)
          ("z"      16 nil eclector.reader:digit-expected)

          ("1"      36 nil 1)
          ("10"     36 nil 36)
          ("a"      36 nil 10)
          ("z"      36 nil 35))))

(macrolet
    ((define-rational-reader-macro-test (char &body cases)
       (let* ((function-name (let ((*package* (find-package '#:eclector.reader)))
                               (alexandria:symbolicate '#:sharpsign- char)))
              (test-name (alexandria:symbolicate function-name '#:/smoke)))
         `(test ,test-name
            ,(format nil "Smoke test for the ~A function." function-name)

            (mapc (lambda (input-parameter-read-suppress-expected)
                    (destructuring-bind
                        (input parameter read-suppress expected
                         &optional (expected-position (length input)))
                        input-parameter-read-suppress-expected
                      (flet ((do-it ()
                               (with-input-from-string (stream input)
                                 (let ((*read-suppress* read-suppress))
                                   (values
                                    (,function-name stream ,char parameter)
                                    (file-position stream))))))
                        (error-case expected
                          (error (do-it))
                          (t
                           (multiple-value-bind (value position) (do-it)
                             (is (equalp expected value))
                             (is (equal expected-position position))))))))
                  ,@cases)))))

  (define-rational-reader-macro-test #\B
    '(;; Errors
      (""     nil nil eclector.reader:end-of-input-before-digit)
      ("1/"   nil nil eclector.reader:end-of-input-before-digit)
      ("2"    nil nil eclector.reader:digit-expected)
      ("x"    nil nil eclector.reader:digit-expected)
      ("1."   nil nil eclector.reader:digit-expected)
      ("1/0"  nil nil eclector.reader:zero-denominator)
      ("1"    1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
      ;; Valid binary rationals
      ("1"    nil nil 1)
      ("-1"   nil nil -1)
      ("1/10" nil nil 1/2)
      ;; With *READ-SUPPRESS* bound to T
      ("1"    1   t   nil)
      ("2"    nil t   nil)
      ("1."   nil t   nil)))

  (define-rational-reader-macro-test #\O
    '(;; Errors
      (""     nil nil eclector.reader:end-of-input-before-digit)
      ("1/"   nil nil eclector.reader:end-of-input-before-digit)
      ("8"    nil nil eclector.reader:digit-expected)
      ("x"    nil nil eclector.reader:digit-expected)
      ("1."   nil nil eclector.reader:digit-expected)
      ("1/0"  nil nil eclector.reader:zero-denominator)
      ("1"    1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
      ;; Valid octal rationals
      ("1"    nil nil 1)
      ("-1"   nil nil -1)
      ("1/10" nil nil 1/8)
      ;; With *READ-SUPPRESS* bound to T
      ("1"    1   t   nil)
      ("8"    nil t   nil)
      ("1."   nil t   nil)))

  (define-rational-reader-macro-test #\X
    '(;; Errors
      (""     nil nil eclector.reader:end-of-input-before-digit)
      ("1/"   nil nil eclector.reader:end-of-input-before-digit)
      ("g"    nil nil eclector.reader:digit-expected)
      ("x"    nil nil eclector.reader:digit-expected)
      ("1."   nil nil eclector.reader:digit-expected)
      ("1/0"  nil nil eclector.reader:zero-denominator)
      ("1"    1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
      ;; Valid hexadecimal rationals
      ("1"    nil nil 1)
      ("-1"   nil nil -1)
      ("1/10" nil nil 1/16)
      ;; With *READ-SUPPRESS* bound to T
      ("1"    1   t   nil)
      ("g"    nil t   nil)
      ("1."   nil t   nil)))

  (define-rational-reader-macro-test #\R
    '(;; Errors
      (""     17  nil eclector.reader:end-of-input-before-digit)
      ("1/"   17  nil eclector.reader:end-of-input-before-digit)
      ("h"    17  nil eclector.reader:digit-expected)
      ("x"    17  nil eclector.reader:digit-expected)
      ("1."   17  nil eclector.reader:digit-expected)
      ("1/0"  17  nil eclector.reader:zero-denominator)
      ("1"    nil nil eclector.reader:numeric-parameter-not-supplied-but-required)
      ("1"    0   nil eclector.reader:invalid-radix)
      ("1"    1   nil eclector.reader:invalid-radix)
      ("1"    37  nil eclector.reader:invalid-radix)
      ;; Valid base-17 rationals
      ("1"    17  nil 1)
      ("-1"   17  nil -1)
      ("1/10" 17  nil 1/17)
      ("g"    17  nil 16)
      ("1 2"  16  nil 1 2)
      ;; With *READ-SUPPRESS* bound to T
      ("h"    17  t   nil)
      ("x"    17  t   nil)
      ("1."   17  t   nil)
      ("1"    nil t   nil)
      ("1"    0   t   nil)
      ("1"    1   t   nil)
      ("1"    37  t   nil))))

(test sharpsign-asterisk/smoke
  "Smoke test for the SHARPSIGN-ASTERISK function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
              (input parameter read-suppress expected
               &optional (expected-position (length input)))
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress))
                         (values
                          (eclector.reader::sharpsign-asterisk stream #\* parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equalp expected value))
                   (is (equal expected-position position))))))))
        '(;; Errors
          ("a"   nil nil eclector.reader:digit-expected)
          ("2"   nil nil eclector.reader:digit-expected)
          (""    2   nil eclector.reader:no-elements-found)
          ("a"   2   nil eclector.reader:digit-expected)
          ("111" 2   nil eclector.reader:too-many-elements)
          ;; Valid
          (""    nil nil #*)
          (" "   nil nil #*)
          (")"   nil nil #*   0)
          ("11"  nil nil #*11)
          ("11 " nil nil #*11)
          ("11)" nil nil #*11 2)
          (""    0   nil #0*)
          ("1"   1   nil #1*1)
          ("1"   2   nil #2*1)
          ("1 "  2   nil #2*1)
          ("1)"  2   nil #2*1 1)
          ;; With *READ-SUPPRESS* bound to T
          (""    nil t   nil)
          ("1"   nil t   nil)
          (""    2   t   nil)
          ("1"   2   t   nil)
          ("11"  2   t   nil)
          ("111" 2   t   nil)
          ("abc" 2   t   nil)
          ("abc" nil t   nil))))

(test sharpsign-vertical-bar/smoke
  "Smoke test for the SHARPSIGN-VERTICAL-BAR function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
              (input parameter read-suppress expected
               &optional (expected-position (length input)))
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress))
                         (values
                          (eclector.reader::sharpsign-vertical-bar
                           stream #\| parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equalp expected value))
                   (is (equal expected-position position))))))))
        '(;; Errors
          (""         nil nil eclector.reader:unterminated-block-comment)
          ("a"        nil nil eclector.reader:unterminated-block-comment)
          ("a|"       nil nil eclector.reader:unterminated-block-comment)
          ("a#||#"    nil nil eclector.reader:unterminated-block-comment)
          ("a|#"      1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ;; Valid
          ("a|#"      nil nil nil)
          ("a# |#"    nil nil nil)
          ("a| |#"    nil nil nil)
          ("a#|b|#|#" nil nil nil)
          ;; With *READ-SUPPRESS* bound to T
          ("a|#"      nil t   nil)
          ("a# |#"    nil t   nil)
          ("a| |#"    nil t   nil)
          ("a#|b|#|#" nil t   nil))))

(test sharpsign-a/smoke
  "Smoke test for the SHARPSIGN-A reader macro function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
              (input parameter read-suppress expected)
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((eclector.reader::*backquote-depth* 1)
                             (*read-suppress* read-suppress))
                         (values
                          (eclector.reader::sharpsign-a stream #\A parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equalp expected value))
                   (is (equal (length input) position))))))))
        '(;; Errors
          (""          1   nil eclector.reader:end-of-input-after-sharpsign-a)
          (")"         1   nil eclector.reader:object-must-follow-sharpsign-a)

          ("(,1)"      1   nil eclector.reader:unquote-in-invalid-context)
          ("(1)"       2   nil eclector.reader:read-object-type-error)
          ("(1 (1))"   2   nil eclector.reader:read-object-type-error)
          ("((1) 1)"   2   nil eclector.reader:read-object-type-error)
          ("(() (1))"  2   nil eclector.reader:incorrect-initialization-length)
          ("1"         1   nil eclector.reader:read-object-type-error)
          ("(1)"       nil nil eclector.reader:numeric-parameter-not-supplied-but-required)
          ;; Valid
          ("()"        0   nil #0A())
          ("(`,1)"     0   nil #0A((eclector.reader:quasiquote
                                    (eclector.reader:unquote 1))))
          ("(1 2)"     0   nil #0A(1 2))
          ("(((1)))"   0   nil #0A(((1))))
          ("()"        1   nil #1A())
          ("(0 0)"     1   nil #1A(0 0))
          ("((1) (2))" 1   nil #1A((1) (2)))
          ("(((1)))"   1   nil #1A(((1))))
          ("((0) (0))" 2   nil #2A((0) (0)))
          ;; With *READ-SUPPRESS* bound to T
          (""          1   t  eclector.reader:end-of-file)
          ("(1)"       2   t  nil)
          ("(() (1))"  2   t  nil)
          ("1"         1   t  nil)
          ("(1)"       nil t  nil)
          ("()"        0   t  nil)
          ("(1 2)"     0   t  nil)
          ("(((1)))"   0   t  nil)
          ("()"        1   t  nil)
          ("(0 0)"     1   t  nil)
          ("((1) (2))" 1   t  nil)
          ("(((1)))"   1   t  nil)
          ("((0) (0))" 2   t  nil))))

(test sharpsign-colon/smoke
  "Smoke test for the SHARPSIGN-COLON reader macro function."

  (mapc (lambda (input-parameter-context-expected)
          (destructuring-bind
              (input parameter read-suppress preserve-whitespace expected
               &optional (expected-position (length input)))
              input-parameter-context-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress)
                             (eclector.reader::*preserve-whitespace*
                               preserve-whitespace))
                         (values
                          (eclector.reader::sharpsign-colon
                           stream #\. parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (string= (symbol-name expected) (symbol-name value)))
                   (is (equal expected-position position))))))))
        '(;; Errors
          ("\\"      nil nil nil eclector.reader:unterminated-single-escape-in-symbol)
          ("|"       nil nil nil eclector.reader:unterminated-multiple-escape-in-symbol)
          ("|\\"     nil nil nil eclector.reader:unterminated-single-escape-in-symbol)
          ("a:b"     nil nil nil eclector.reader:uninterned-symbol-must-not-contain-package-marker)
          ("a:b:c"   nil nil nil eclector.reader:uninterned-symbol-must-not-contain-package-marker)
          ("a"       1   nil nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ;; Valid
          (""        nil nil nil #:||)
          (" "       nil nil nil #:||)
          ("("       nil nil nil #:|| 0)
          ("\\a"     nil nil nil #:|a|)
          ("\\:"     nil nil nil #:|:|)
          ("|\\a|"   nil nil nil #:|a|)
          ("|a|"     nil nil nil #:|a|)
          ("|a:|"    nil nil nil #:|a:|)
          ;; *PRESERVE-WHITESPACE*
          ("a "      nil nil nil #:a)
          ("a "      nil nil t   #:a 1)
          ;; With *READ-SUPPRESS* bound to T
          ("\\"      nil t   nil eclector.reader:unterminated-single-escape-in-symbol)
          ("|"       nil t   nil eclector.reader:unterminated-multiple-escape-in-symbol)
          ("a"       1   t   nil nil))))

(test sharpsign-c/smoke
  "Smoke test for the SHARPSIGN-C reader macro function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
              (input parameter read-suppress expected)
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress)
                             (eclector.reader::*backquote-depth* 1))
                         (values
                          (eclector.reader::sharpsign-c stream #\C parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equal expected value))
                   (is (equal (length input) position))))))))
        '(;; Errors
          (""        nil nil eclector.reader:end-of-input-after-sharpsign-c)
          (")"       nil nil eclector.reader:complex-parts-must-follow-sharpsign-c)
          ("\"foo\"" nil nil eclector.reader:non-list-following-sharpsign-c)
          ("("       nil nil eclector.reader:end-of-input-before-complex-part)
          ("()"      nil nil eclector.reader:complex-part-expected)
          ("(0"      nil nil eclector.reader:end-of-input-before-complex-part)
          ("(0)"     nil nil eclector.reader:complex-part-expected)
          ("(0 0"    nil nil eclector.reader:unterminated-list)
          ("(0 0 0)" nil nil eclector.reader:too-many-complex-parts)
          ("#(0 0)"  nil nil eclector.reader:non-list-following-sharpsign-c)
          ("(:a 0)"  nil nil eclector.reader:read-object-type-error)
          ("(0 :a)"  nil nil eclector.reader:read-object-type-error)
          ("(0 0)"   1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ("(`,0 0)" nil nil eclector.reader:backquote-in-invalid-context)
          ("(,0 0)"  nil nil eclector.reader:unquote-in-invalid-context)
          ;; Valid
          ("(0 1)"   nil nil #C(0 1))
          ("(-1 1)"  nil nil #C(-1 1))
          ("(0 1/2)" nil nil #C(0 1/2))
          ;; With *READ-SUPPRESS* bound to T
          ("(0)"     nil t   nil)
          ("(0 0 0)" nil t   nil)
          ("#(0 0)"  nil t   nil)
          ("(:a 0)"  nil t   nil)
          ("(0 :a)"  nil t   nil)
          ("(0 0)"   nil t   nil))))

(test sharpsign-s/smoke
  "Smoke test for the SHARPSIGN-S reader macro function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
              (input parameter read-suppress expected)
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress)
                             (eclector.reader::*backquote-depth* 1)
                             (eclector.reader::*client*
                               (make-instance 'sharpsign-s-client)))
                         (values
                          (eclector.reader::sharpsign-s stream #\S parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (relaxed-equalp expected       value))
                   (is (equal          (length input) position))))))))
        '(;; Errors
          ("(foo)"          1   nil eclector.reader:numeric-parameter-supplied-but-ignored)

          (""               nil nil eclector.reader:end-of-input-after-sharpsign-s)
          ("1"              nil nil eclector.reader:non-list-following-sharpsign-s)
          (")"              nil nil eclector.reader:structure-constructor-must-follow-sharpsign-s)
          ("`"              nil nil eclector.reader:backquote-in-invalid-context)
          ("(foo . 1)"      nil nil eclector.reader:invalid-context-for-consing-dot)

          ("("              nil nil eclector.reader:end-of-input-before-structure-type-name)
          ("()"             nil nil eclector.reader:no-structure-type-name-found)
          ("(1)"            nil nil eclector.reader:structure-type-name-is-not-a-symbol)

          ("(foo"           nil nil eclector.reader:end-of-input-before-slot-name)
          ("(foo 1 2)"      nil nil eclector.reader:slot-name-is-not-a-string-designator)
          ("(foo :bar"      nil nil eclector.reader:end-of-input-before-slot-value)
          ("(foo :bar)"     nil nil eclector.reader:no-slot-value-found)
          ("(foo :bar 1"    nil nil eclector.reader:end-of-input-before-slot-name)

          ("(`,foo :bar 1)" nil nil eclector.reader:backquote-in-invalid-context)
          ("(,foo :bar 1)"  nil nil eclector.reader:unquote-in-invalid-context)
          ("(foo `,:bar 1)" nil nil eclector.reader:backquote-in-invalid-context)
          ("(foo ,:bar 1)"  nil nil eclector.reader:unquote-in-invalid-context)
          ("(foo :bar ,1)"  nil nil eclector.reader:unquote-in-invalid-context)
          ;; Valid
          ("(foo)"          nil nil (foo))
          (" (foo)"         nil nil (foo)) ; unclear in the spec, but we allow it
          ("(foo #:bar 1)"  nil nil (foo #:bar 1))
          ("(foo :bar 1)"   nil nil (foo :bar 1))
          ("(foo bar 1)"    nil nil (foo bar 1))
          ("(foo \"bar\" 1)"nil nil (foo "bar" 1))
          ("(foo #\\b 1)"   nil nil (foo #\b 1))
          ("(foo :bar `,1)" nil nil (foo :bar (eclector.reader:quasiquote
                                               (eclector.reader:unquote 1))))
          ;; With *READ-SUPPRESS* bound to T
          ("(foo)"          nil t   nil)
          ("(foo 1 2)"      nil t   nil)
          ("(foo :bar)"     nil t   nil)
          ("(foo)"          1   t   nil))))

(test sharpsign-p/smoke
  "Smoke test for the SHARPSIGN-P reader macro function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
                (input parameter read-suppress expected)
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress)
                             (eclector.reader::*backquote-depth* 1))
                         (values
                          (eclector.reader::sharpsign-p stream #\P parameter)
                          (file-position stream))))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equal expected value))
                   (is (equal (length input) position))))))))
        '(;; Errors
          ("\"foo\""   1   nil eclector.reader:numeric-parameter-supplied-but-ignored)

          (""          nil nil eclector.reader:end-of-input-after-sharpsign-p)
          ("1"         nil nil eclector.reader:non-string-following-sharpsign-p)
          (")"         nil nil eclector.reader:namestring-must-follow-sharpsign-p)
          ("`,\"foo\"" nil nil eclector.reader:backquote-in-invalid-context)
          (",\"foo\""  nil nil eclector.reader:unquote-in-invalid-context)
          ;; Valid
          ("\"foo\""   nil nil #P"foo")
          ;; With *READ-SUPPRESS* bound to T
          ("\"foo\""   nil t   nil)
          ("\"foo\""   1   t   nil))))

(test sharpsign-plus-minus/smoke
  "Smoke test for the SHARPSIGN-{PLUS,MINUS} functions."

  (mapc (lambda (input-parameter-context-expected)
          (destructuring-bind
              (input parameter *read-suppress*
               plus-expected &optional (minus-expected plus-expected))
              input-parameter-context-expected
            (labels ((do-it (which)
                       (with-input-from-string (stream input)
                         (let ((eclector.reader::*input-stream* stream)
                               (eclector.reader::*backquote-depth* 1))
                           (values (ecase which
                                     (:plus  (eclector.reader::sharpsign-plus
                                              stream #\+ parameter))
                                     (:minus (eclector.reader::sharpsign-minus
                                              stream #\- parameter)))
                                   (file-position stream)))))
                     (do-one (which expected)
                       (error-case expected
                         (error (do-it which))
                         (t
                          (multiple-value-bind (value position) (do-it which)
                            (is (equal expected value))
                            (is (equal (length input) position)))))))
              (do-one :plus  plus-expected)
              (do-one :minus minus-expected))))
        '(;; Errors
          (""                      nil nil eclector.reader:end-of-input-after-sharpsign-plus-minus)
          (")"                     nil nil eclector.reader:feature-expression-must-follow-sharpsign-plus-minus)
          ("(and)"                 nil nil eclector.reader:end-of-input-after-feature-expression)
          ("(and))"                nil nil eclector.reader:object-must-follow-feature-expression)

          ("1"                     nil nil eclector.reader:feature-expression-type-error)
          ("(1)"                   nil nil eclector.reader:feature-expression-type-error)
          ("(not :foo :bar)"       nil nil eclector.reader:single-feature-expected)
          ("(not 1)"               nil nil eclector.reader:feature-expression-type-error)
          ("(and 1)"               nil nil eclector.reader:feature-expression-type-error)
          ("(or 1)"                nil nil eclector.reader:feature-expression-type-error)

          ("(and) 1"               1   nil eclector.reader:numeric-parameter-supplied-but-ignored)

          ("`,(and)"               nil nil eclector.reader:backquote-in-invalid-context)
          (",(and)"                nil nil eclector.reader:unquote-in-invalid-context)
          ;; Valid
          ("common-lisp 1"         nil nil 1   nil)
          ("(not common-lisp) 1"   nil nil nil 1)
          ("(and) 1"               nil nil 1   nil)
          ("(or) 1"                nil nil nil 1)
          ("(not (not (and))) 1"   nil nil 1   nil)
          ;; With *READ-SUPPRESS* bound to T
          ("(and) 1"               nil t   nil nil)
          ("(and) 1"               1   t   nil nil)
          ;; In which package is the guarded expression read?
          ("(and) foo"             nil nil foo nil)
          ;; Vendor extension shouldn't break other implementations
          ;; when guarded properly
          ("(and some-lisp
                 (version> \"1\"))
            1"                     nil nil nil 1))))

(test sharpsign-invalid/smoke
  "Smoke test for the SHARPSIGN-INVALID function."

  (signals-printable eclector.reader:sharpsign-invalid
    (with-input-from-string (stream "")
      (eclector.reader::sharpsign-invalid stream #\< nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign equals and sharpsign sharpsign.

(test sharpsign-{equal\,sharpsign}/smoke
  "Smoke test for the SHARPSIGN-{EQUAL,SHARPSIGN} functions."

  (mapc (lambda (input-expected)
          (destructuring-bind (input read-suppress expected) input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress))
                         (eclector.reader:read stream)))))
              (error-case expected
                (error (do-it))
                (recursive-cons
                 (let ((result (do-it)))
                   (is-true (consp result))
                   (is (eq result (car result)))))
                (t
                 (is (equalp expected (do-it))))))))
        '(;; sharpsign equals errors
          ("#1="            nil eclector.reader:end-of-input-after-sharpsign-equals)
          ("(#1=)"          nil eclector.reader:object-must-follow-sharpsign-equals)
          ("#="             nil eclector.reader:numeric-parameter-not-supplied-but-required)
          ("(#1=1 #1=2)"    nil eclector.reader:sharpsign-equals-label-defined-more-than-once)
          ("#1=#1#"         nil eclector.reader:sharpsign-equals-only-refers-to-self)
          ;; sharpsign sharpsign errors
          ("##"             nil eclector.reader:numeric-parameter-not-supplied-but-required)
          ("#1#"            nil eclector.reader:sharpsign-sharpsign-undefined-label)
          ("(#1=1 #2#)"     nil eclector.reader:sharpsign-sharpsign-undefined-label)
          ;;
          ("(#1=1)"         nil (1))
          ("(#1=1 #1#)"     nil (1 1))
          ("(#1=1 #1# #1#)" nil (1 1 1))
          ("#1=(#1#)"       nil recursive-cons)
          ;; There was problem leading to unrelated expressions of the
          ;; forms (nil) and (t) being replaced by the fixup
          ;; processor.
          ("(#1=1 (nil))"   nil (1 (nil)))
          ("(#1=((nil)))"   nil (((nil))))
          ("(#1=1 (t))"     nil (1 (t)))
          ("(#1=((t)))"     nil (((t))))
          ;; With *READ-SUPPRESS* bound to t
          ("#=1"            t   nil)
          ("(#1=1 #1=2)"    t   nil)
          ("##"             t   nil)
          ("#1#"            t   nil))))
