(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.tokens
    :in :eclector.reader)

(test read-token/smoke
  "Smoke test for the default method on READ-TOKEN."

  (mapc (lambda (input-args-expected)
          (destructuring-bind
              (input eof-error-p eof-value preserve-whitespace
               expected &optional (expected-position (length input)))
              input-args-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((*package*
                                       (find-package '#:eclector.reader.test)) ; TODO use a client that does not intern in INTERPRET-TOKEN
                                     (eclector.reader:*preserve-whitespace*
                                       preserve-whitespace))
                                 (eclector.reader:read-token
                                  t stream eof-error-p eof-value))
                               (file-position stream)))))
              (case expected
                (eclector.reader:end-of-file
                 (signals eclector.reader:end-of-file (do-it)))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equalp expected          value))
                   (is (eql    expected-position position))))))))
        '(("a"      t   nil nil |A|)
          ("\\"     t   nil nil eclector.reader:end-of-file)
          ("\\"     nil nil nil nil)
          ("\\a"    t   nil nil |a|)

          ("aa"     t   nil nil |AA|)
          ("a#"     t   nil nil |A#|)
          ("a\\"    t   nil nil eclector.reader:end-of-file)
          ("a\\"    nil nil nil nil)
          ("a\\a"   t   nil nil |Aa|)
          ("a|a|"   t   nil nil |Aa|)
          ("a,"     t   nil nil |A|  1)
          ("a "     t   nil nil |A|)
          ("a "     t   nil t   |A|  1)

          ("|"      t   nil nil eclector.reader:end-of-file)
          ("|"      nil nil nil nil)
          ("||"     t   nil nil ||)
          ("||a"    t   nil nil |A|)
          ("|a|"    t   nil nil |a|)
          ("|#|"    t   nil nil |#|)
          ("|,|"    t   nil nil |,|)
          ("| |"    t   nil nil | |)
          ("|\\"    t   nil nil eclector.reader:end-of-file)
          ("|\\"    nil nil nil nil)
          ("|\\|"   t   nil nil eclector.reader:end-of-file)
          ("|\\|"   nil nil nil nil)
          ("|\\||"  t   nil nil |\||)

          (".\\."   t   nil nil |..|)
          (".||."   t   nil nil |..|)
          ("..a"    t   nil nil |..A|)

          ("\\a||"  t   nil nil |a|)
          ("\\a||b" t   nil nil |aB|))))

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
                (case expected
                  (eclector.reader:symbol-name-must-not-end-with-package-marker
                   (signals eclector.reader:symbol-name-must-not-end-with-package-marker
                     (do-it)))
                  (eclector.reader:package-does-not-exist
                   (signals eclector.reader:package-does-not-exist (do-it)))
                  (eclector.reader:symbol-does-not-exist
                   (signals eclector.reader:symbol-does-not-exist (do-it)))
                  (eclector.reader:symbol-is-not-external
                   (signals eclector.reader:symbol-is-not-external (do-it)))
                  (t
                   (is (equal expected (do-it)))))))))
        '((""                               nil nil nil ||)
          ("a"                              nil nil nil |a|)
          ("A"                              nil nil nil a)
          ("A:"                             1   nil nil eclector.reader:symbol-name-must-not-end-with-package-marker)

          (":"                              0   nil nil eclector.reader:symbol-name-must-not-end-with-package-marker)
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

(test interpret-token.default/smoke
  "Smoke test for the default method on INTERPRET-TOKEN."

  (mapc (lambda (arguments-context-expected)
          (destructuring-bind
              (token token-escapes *read-base* case expected)
              arguments-context-expected
            (let ((table (eclector.readtable:copy-readtable
                          eclector.reader:*readtable*)))
              (setf (eclector.readtable:readtable-case table) case)
              (flet ((do-it ()
                       (with-input-from-string (stream "")
                         (let ((eclector.reader:*readtable* table)
                               (*read-default-float-format* 'single-float))
                           (eclector.reader:interpret-token
                            nil stream (copy-seq token) token-escapes)))))
                (case expected
                  (eclector.reader:invalid-context-for-consing-dot
                   (signals eclector.reader:invalid-context-for-consing-dot
                     (do-it)))
                  (eclector.reader:too-many-dots
                   (signals eclector.reader:too-many-dots
                     (do-it)))
                  (eclector.reader:symbol-name-must-not-end-with-package-marker
                   (signals eclector.reader:symbol-name-must-not-end-with-package-marker
                     (do-it)))
                  (eclector.reader:symbol-can-have-at-most-two-package-markers
                   (signals eclector.reader:symbol-can-have-at-most-two-package-markers
                     (do-it)))
                  (eclector.reader:two-package-markers-must-be-adjacent
                   (signals eclector.reader:two-package-markers-must-be-adjacent
                     (do-it)))
                  (eclector.reader:two-package-markers-must-not-be-first
                   (signals eclector.reader:two-package-markers-must-not-be-first
                     (do-it)))
                  (t
                   (unless (or token-escapes (zerop (length token)))
                     (assert (equal expected
                                    (let ((*readtable* (copy-readtable))
                                          (*read-default-float-format* 'single-float))
                                      (setf (readtable-case *readtable*) case)
                                      (read-from-string token)))))
                   (is (equal expected (do-it)))))))))
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
          ("a:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker)
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
          ("+.234"      ()                10 :upcase       .234f0)
          ("-.234"      ()                10 :upcase      -.234f0)
          (".234"       ()                10 :upcase       .234f0)
          ("+1.234"     ()                10 :upcase      1.234f0)
          ("-1.234"     ()                10 :upcase     -1.234f0)
          ("1.234"      ()                10 :upcase      1.234f0)

          ;; float-exponent
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
          ("a"          ( )               16 :upcase     10)
          ("-a"         ( )               16 :upcase    -10)
          ("1a"         ( )               16 :upcase     26)
          ("1aa"        ( )               16 :upcase    426))))
