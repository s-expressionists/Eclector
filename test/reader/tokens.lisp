(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.tokens
  :in :eclector.reader)

(test read-token/smoke
  "Smoke test for the default method on READ-TOKEN."

  (do-stream-input-cases ((length) eof-error-p eof-value
                          expected &optional (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*package* (find-package '#:eclector.reader.test))) ; TODO use a client that does not intern in INTERPRET-TOKEN
                 (eclector.reader:read-token
                  t stream eof-error-p eof-value)))))
      (error-case expected
        (error (do-it))
        (t
         (multiple-value-bind (value position) (do-it)
           (expect "value"    (equalp expected          value))
           (expect "position" (eql    expected-position position))))))
    `((,(format nil "~C" #\Backspace)   t   nil eclector.reader:invalid-constituent-character)
      (,(format nil "~C" #\Rubout)      t   nil eclector.reader:invalid-constituent-character)
      ("a"                              t   nil |A|)
      ("\\"                             t   nil eclector.reader:unterminated-single-escape-in-symbol)
      ("\\"                             nil nil eclector.reader:unterminated-single-escape-in-symbol)
      ("\\a"                            t   nil |a|)
      (,(format nil "\\~C" #\Backspace) t   nil ,(intern (format nil "~C" #\Backspace)))

      (,(format nil "a~C" #\Backspace)  t   nil eclector.reader:invalid-constituent-character)
      (,(format nil "a~C" #\Rubout)     t   nil eclector.reader:invalid-constituent-character)
      ("aa"                             t   nil |AA|)
      ("a#"                             t   nil |A#|)
      ("a\\"                            t   nil eclector.reader:unterminated-single-escape-in-symbol)
      ("a\\"                            nil nil eclector.reader:unterminated-single-escape-in-symbol)
      ("a\\a"                           t   nil |Aa|)
      ("a|a|"                           t   nil |Aa|)
      ("a,"                             t   nil |A|  1)
      ("a "                             t   nil |A|  1)

      ("|"                              t   nil eclector.reader:unterminated-multiple-escape-in-symbol)
      ("|"                              nil nil eclector.reader:unterminated-multiple-escape-in-symbol)
      ("||"                             t   nil ||)
      ("||a"                            t   nil |A|)
      ("|a|"                            t   nil |a|)
      ("|#|"                            t   nil |#|)
      ("|,|"                            t   nil |,|)
      ("| |"                            t   nil | |)
      (,(format nil "|~C|" #\Backspace) t   nil ,(intern (format nil "~C" #\Backspace)))
      ("|\\"                            t   nil eclector.reader:unterminated-single-escape-in-symbol)
      ("|\\"                            nil nil eclector.reader:unterminated-single-escape-in-symbol)
      ("|\\|"                           t   nil eclector.reader:unterminated-multiple-escape-in-symbol)
      ("|\\|"                           nil nil eclector.reader:unterminated-multiple-escape-in-symbol)
      ("|\\||"                          t   nil |\||)

      (".\\."                           t   nil |..|)
      (".||."                           t   nil |..|)
      (".||"                            t   nil |.|)
      ("..a"                            t   nil |..A|)

      ("\\a||"                          t   nil |a|)
      ("\\a||b"                         t   nil |aB|))))

(test interpet-symbol-token/smoke
  "Smoke test for the default method on INTERPRET-SYMBOL-TOKEN."

  (mapc (lambda (arguments-package-expected)
          (destructuring-bind (token marker1 marker2 package expected)
              arguments-package-expected
            (let ((*package* (or package *package*)))
              (flet ((do-it ()
                       (with-input-from-string (stream "")
                         (eclector.reader:interpret-symbol-token
                          nil stream token marker1 marker2))))
                (error-case expected
                  (error (do-it))
                  (t
                   (is (equal expected (do-it)))))))))
        '((""                               nil nil nil ||)
          ("a"                              nil nil nil |a|)
          ("A"                              nil nil nil a)
          ("A:"                             1   nil nil eclector.reader:symbol-name-must-not-end-with-package-marker)
          ("A::"                            2   nil nil eclector.reader:symbol-name-must-not-end-with-package-marker)

          (":"                              nil nil nil |:|)
          (":"                              0   nil nil :||)
          (":a"                             0   nil nil :|a|)
          (":A"                             0   nil nil :a)
          ("NP:NIX"                         2   nil nil eclector.reader:package-does-not-exist)
          ("CL:NIX"                         2   nil nil eclector.reader:symbol-does-not-exist)
          ("ECLECTOR.READER.TEST:INTERNAL"  20  nil nil eclector.reader:symbol-is-not-external)
          ("CL:NIL"                         2   nil nil nil)
          ("CL:ABS"                         2   nil nil abs)

          ("::"                             0   1   nil eclector.reader:symbol-name-must-not-end-with-package-marker)
          ("NP::NIX"                        2   3   nil eclector.reader:package-does-not-exist)
          ("ECLECTOR.READER.TEST::INTERNAL" 20  21  nil internal)
          ("CL::NIL"                        2   3   nil nil)
          ("CL::ABS"                        2   3   nil abs))))

(defun do-interpret-token-test-case (arguments-context-expected)
  (destructuring-bind (token token-escapes *read-base* case expected)
      arguments-context-expected
    (let ((table (eclector.readtable:copy-readtable
                  eclector.reader:*readtable*)))
      (setf (eclector.readtable:readtable-case table) case)
      (flet ((do-it ()
               (with-input-from-string (stream "")
                 (let ((eclector.reader:*readtable* table))
                   (eclector.reader:interpret-token
                    nil stream (copy-seq token) token-escapes)))))
        (error-case expected
          (error (do-it))
          (t
           (unless (or token-escapes (zerop (length token)))
             (assert (equal expected
                            (let ((*readtable* (copy-readtable))
                                  (*read-default-float-format* 'single-float))
                              (setf (readtable-case *readtable*) case)
                              (read-from-string token)))))
           (is (equal expected (do-it)))))))))

(test interpret-token.default/smoke
  "Smoke test for the default method on INTERPRET-TOKEN."

  (let ((*read-default-float-format* 'single-float))
    (mapc #'do-interpret-token-test-case
          '(;; empty
            (""           ()                10 :upcase   ||)

            ;; Empty escape ranges
            ("a"          ((0 . 0))         10 :upcase   |A|)
            ("a"          ((1 . 1))         10 :upcase   |A|)

            ("abc"        ((0 . 0) (1 . 2)) 10 :upcase   |AbC|)
            ("abc"        ((0 . 0) (1 . 2)) 10 :downcase |abc|)
            ("abc"        ((0 . 0) (1 . 2)) 10 :preserve |abc|)
            ("abc"        ((0 . 0) (1 . 2)) 10 :invert   |AbC|)

            ;; "consing dot"
            ("."          ()                10 :upcase   eclector.reader:invalid-context-for-consing-dot)
            (".."         ((1 . 2))         10 :upcase   |..|) ; .\.
            (".."         ((1 . 1))         10 :upcase   |..|) ; .||.
            ("."          ((1 . 1))         10 :upcase   |.|)  ; .||
            ("..a"        ()                10 :upcase   |..A|)
            (".."         ()                10 :upcase   eclector.reader:too-many-dots)
            ("..."        ()                10 :upcase   eclector.reader:too-many-dots)

            ;; readtable case
            ("abc"        ()                10 :upcase   |ABC|)
            ("ABC"        ()                10 :upcase   |ABC|)
            ("aBc"        ()                10 :upcase   |ABC|)

            ("abc"        ()                10 :downcase |abc|)
            ("ABC"        ()                10 :downcase |abc|)
            ("aBc"        ()                10 :downcase |abc|)

            ("abc"        ()                10 :preserve |abc|)
            ("ABC"        ()                10 :preserve |ABC|)
            ("aBc"        ()                10 :preserve |aBc|)

            ("abc"        ()                10 :invert   |ABC|)
            ("ABC"        ()                10 :invert   |abc|)
            ("aBc"        ()                10 :invert   |aBc|)

            ;; symbol
            ("a"          ((0 . 1))         10 :upcase   |a|)
            ("-"          ()                10 :upcase   -)
            ("+"          ()                10 :upcase   +)

            ("-a"         ((1 . 2))         10 :upcase   |-a|)
            ("-."         ()                10 :upcase   -.)
            ("-:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("-a"         ()                10 :upcase   -a)

            (".a"         ((1 . 2))         10 :upcase   |.a|)
            (".:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            (".a"         ()                10 :upcase   .a)

            ("-.a"        ((2 . 3))         10 :upcase   |-.a|)
            ("-.:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("-.a"        ()                10 :upcase   -.a)

            ("1a"         ()                10 :upcase   1a)
            ("1a"         ((1 . 2))         10 :upcase   |1a|)
            ("1:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("1.a"        ((2 . 3))         10 :upcase   |1.a|)
            ("1.:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("1.a"        ()                10 :upcase   1.a)

            ("1/"         ()                10 :upcase   1/)
            ("1/a"        ((2 . 3))         10 :upcase   |1/a|)
            ("1/:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("1/a"        ()                10 :upcase   1/a)
            ("1/0"        ()                10 :upcase   eclector.reader:zero-denominator)
            ("1/2a"       ((3 . 4))         10 :upcase   |1/2a|)
            ("1/2:"       ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("1/2a"       ()                10 :upcase   1/2a)

            (".1a"        ((2 . 3))         10 :upcase   |.1a|)
            (".1:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            (".1a"        ()                10 :upcase   .1a)

            (".1e"        ()                10 :upcase   .1e)
            (".1ea"       ((3 . 4))         10 :upcase   |.1Ea|)
            (".1e:"       ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            (".1ea"       ()                10 :upcase   .1ea)
            (".1e-"       ()                10 :upcase   .1e-)
            (".1e+"       ()                10 :upcase   .1e+)
            (".1e-a"      ((4 . 5))         10 :upcase   |.1E-a|)
            (".1e-:"      ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            (".1e-a"      ()                10 :upcase   .1e-a)
            (".1e2a"      ((4 . 5))         10 :upcase   |.1E2a|)
            (".1e2:"      ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            (".1e2a"      ()                10 :upcase   .1e2a)

            ("1aa"        ((2 . 3))         16 :upcase   |1Aa|)
            ("1a/"        ()                16 :upcase   1a/)
            ("1a:"        ()                16 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("1ax"        ()                16 :upcase   1ax)

            ("aa"         ((1 . 2))         10 :upcase   |Aa|)
            (":"          ()                10 :upcase   eclector.reader:symbol-name-must-not-be-only-package-markers)
            (":"          ((1 . 1))         10 :upcase   :||)
            (":"          ((0 . 1))         10 :upcase   |:|)
            ("::"         ()                10 :upcase   eclector.reader:two-package-markers-must-not-be-first)
            ("a:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("a:"         ((1 . 2))         10 :upcase   |A:|)
            ("a::"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
            ("a:::"       ()                10 :upcase   eclector.reader:symbol-can-have-at-most-two-package-markers)
            ("a:a:"       ()                10 :upcase   eclector.reader:two-package-markers-must-be-adjacent)
            ("::a"        ()                10 :upcase   eclector.reader:two-package-markers-must-not-be-first)
            ("keyword:b"  ()                10 :upcase   :b)
            ("keyword::b" ()                10 :upcase   :b)

            ;; decimal-integer
            ("1"          ()                10 :upcase      1)
            ("-1"         ()                10 :upcase     -1)
            ("-12"        ()                10 :upcase    -12)

            ("1."         ()                10 :upcase      1)

            ;; ratio
            ("+1/2"       ()                10 :upcase      1/2)
            ("-1/2"       ()                10 :upcase     -1/2)
            ("1/2"        ()                10 :upcase      1/2)
            ("1/23"       ()                10 :upcase      1/23)

            ;; float-no-exponent
            ("-.0"        ()                10 :upcase      -.0f0)
            ("+.0"        ()                10 :upcase      +.0f0)
            ("+.234"      ()                10 :upcase       .234f0)
            ("-.234"      ()                10 :upcase      -.234f0)
            (".234"       ()                10 :upcase       .234f0)
            ("-0.0"       ()                10 :upcase      -.0f0)
            ("+0.0"       ()                10 :upcase      +.0f0)
            ("+1.234"     ()                10 :upcase      1.234f0)
            ("-1.234"     ()                10 :upcase     -1.234f0)
            ("1.234"      ()                10 :upcase      1.234f0)

            ;; float-exponent
            ("-.0e1"      ()                10 :upcase      -.0f0)
            ("+.0e1"      ()                10 :upcase      +.0f0)
            ("-0.0e1"     ()                10 :upcase      -.0f0)
            ("+0.0e1"     ()                10 :upcase      +.0f0)
            ("+1.0e2"     ()                10 :upcase    100.0f0)
            ("+1.0e-2"    ()                10 :upcase      0.01f0)
            ("-1.0e2"     ()                10 :upcase   -100.0f0)
            ("-1.0e-2"    ()                10 :upcase     -0.01f0)
            ("1.0e2"      ()                10 :upcase    100.0f0)
            ("1.0e-2"     ()                10 :upcase      0.01f0)
            ("1.e2"       ()                10 :upcase    100.0f0)
            ("1e2"        ()                10 :upcase    100.0f0)
            ("1e01"       ()                10 :upcase     10.0f0)

            ;; Nondefault *READ-BASE*
            ("2"          ()                2  :upcase    |2|)
            ("2/1"        ()                2  :upcase    |2/1|)
            ("2."         ()                2  :upcase    2)

            ("a"          ()                16 :upcase     10)
            ("-a"         ()                16 :upcase    -10)
            ("1a"         ()                16 :upcase     26)
            ("1aa"        ()                16 :upcase    426)

            ("10."        ()                16 :upcase    10)

            ("1111.1111"  ()                4  :upcase    1111.1111f0)
            ("1111.1234"  ()                4  :upcase    1111.1234f0)
            ("1111.4321"  ()                4  :upcase    1111.4321f0)
            ("1111.4444"  ()                4  :upcase    1111.4444f0)

            ("1234.1111"  ()                4  :upcase    1234.1111f0)
            ("1234.1234"  ()                4  :upcase    1234.1234f0)
            ("1234.4321"  ()                4  :upcase    1234.4321f0)
            ("1234.4444"  ()                4  :upcase    1234.4444f0)

            ("4321.1111"  ()                4  :upcase    4321.1111f0)
            #-ccl ("4321.1234"  ()                4  :upcase    4321.1234f0)
            ("4321.4321"  ()                4  :upcase    4321.4321f0)
            ("4321.4444"  ()                4  :upcase    4321.4444f0)

            ("4444.1111"  ()                4  :upcase    4444.1111f0)
            ;; TODO CCL's FLOAT is different:
            ;;
            ;;   (float (/ (+ 44440000 1234) 10000) 1.0f0) => 4444.123 (not 4444.1235)
            ;;
            ;; Maybe implement a different algorithm (SCALE-FLOAT?) for
            ;; building float results.
            ;;
            ;; Note that CCL's reader produces the "right" result
            ;; (i.e. 4444.1235) so there has to be a trick to do it in
            ;; CCL.
            #-ccl ("4444.1234"  ()                4  :upcase    4444.1234f0)
            ("4444.4321"  ()                4  :upcase    4444.4321f0)
            ("4444.4444"  ()                4  :upcase    4444.4444f0)))))

;;; Binding *READ-DEFAULT-FLOAT-FORMAT* to a non-standard value is
;;; allowed if the implementation accepts the value. SBCL allows the
;;; specific value RATIONAL. CCL doesn't seem to type-check the value
;;; so we can get away with it. Other implementations could signal an
;;; error.
#+(or sbcl ccl)
(test interpret-token.default/default-float-format
  "Test default float format handling in the default method on INTERPRET-TOKEN."

  (let ((*read-default-float-format* 'rational))
    (mapc #'do-interpret-token-test-case
          '(("1.0" () 10 :upcase eclector.reader:invalid-default-float-format)
            ("1e0" () 10 :upcase eclector.reader:invalid-default-float-format)))))
