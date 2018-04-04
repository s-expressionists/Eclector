(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.tokens
    :in :eclector.reader)

(test read-token.smoke
  "Smoke test for the default method on READ-TOKEN."

  (map nil (lambda (input-args-expected)
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
                   (end-of-file
                    (signals end-of-file (do-it)))
                   (t
                    (multiple-value-bind (value position) (do-it)
                      (is (equalp expected          value))
                      (is (eql    expected-position position))))))))
       '(("a"     t   nil nil |A|)
         ("\\"    t   nil nil end-of-file)
         ("\\"    nil nil nil nil)
         ("\\a"   t   nil nil |a|)
         ("|a|"   t   nil nil |a|)

         ("aa"    t   nil nil |AA|)
         ("a#"    t   nil nil |A#|)
         ("a\\"   t   nil nil end-of-file)
         ("a\\"   nil nil nil nil)
         ("a\\a"  t   nil nil |Aa|)
         ("a|a|"  t   nil nil |Aa|)
         ("a,"    t   nil nil |A|  1)
         ("a "    t   nil nil |A|)
         ("a "    t   nil t   |A|  1)

         ("|"     t   nil nil end-of-file)
         ("|"     nil nil nil nil)
         ("|a|"   t   nil nil |a|)
         ("|#|"   t   nil nil |#|)
         ("|,|"   t   nil nil |,|)
         ("| |"   t   nil nil | |)
         ("|\\"   t   nil nil end-of-file)
         ("|\\"   nil nil nil nil)
         ("|\\|"  t   nil nil end-of-file)
         ("|\\|"  nil nil nil nil)
         ("|\\||" t   nil nil |\||))))

(test interpet-symbol.smoke
  "Smoke test for the default method on INTERPRET-SYMBOL."

  (map nil (lambda (arguments-package-expected)
             (destructuring-bind (token marker1 marker2 package expected)
                 arguments-package-expected
               (let ((*package* (or package *package*)))
                 (flet ((do-it ()
                          (with-input-from-string (stream "")
                            (eclector.reader:interpret-symbol
                             token marker1 marker2 stream))))
                   (case expected
                     (eclector.reader:symbol-name-must-not-end-with-package-marker
                      (signals eclector.reader:symbol-name-must-not-end-with-package-marker
                        (do-it)))
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
         ;; ("NP:NIX"                         2   nil nil package-does-not-exist)
         ("CL:NIX"                         2   nil nil eclector.reader:symbol-does-not-exist)
         ("ECLECTOR.READER.TEST:INTERNAL"  20  nil nil eclector.reader:symbol-is-not-external)
         ("CL:NIL"                         2   nil nil nil)
         ("CL:ABS"                         2   nil nil abs)

         ("::"                             0   1   nil eclector.reader:symbol-name-must-not-end-with-package-marker)
         ;; ("NP::NIX"                        2   3   nil package-does-not-exist)
         ("ECLECTOR.READER.TEST::INTERNAL" 20  21  nil internal)
         ("CL::NIL"                        2   3   nil nil)
         ("CL::ABS"                        2   3   nil abs))))

(test interpret-token.default.smoke
  "Smoke test for the default method on INTERPRET-TOKEN."

  (map nil (lambda (arguments-context-expected)
             (destructuring-bind
                   (token maybe-token-escapes *read-base* expected)
                 arguments-context-expected
               (let ((token-escapes (if maybe-token-escapes
                                        (map 'vector #'plusp maybe-token-escapes)
                                        (make-array (length token)
                                                    :initial-element nil))))
                 (flet ((do-it ()
                          (with-input-from-string (stream "")
                            (eclector.reader:interpret-token
                             (copy-seq token) token-escapes stream))))
                   (case expected
                     (eclector.reader:invalid-context-for-consing-dot
                      (signals eclector.reader:invalid-context-for-consing-dot
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
                      (unless (or maybe-token-escapes (zerop (length token)))
                        (assert (equal expected (read-from-string token))))
                      (is (equal expected (do-it)))))))))
       '(;; empty
         (""           nil     10 ||)

         ;; "consing dot"
         ("."          nil     10 eclector.reader:invalid-context-for-consing-dot)

         ;; symbol
         ("a"          #*1     10 |a|)
         ("-"          nil     10 -)
         ("+"          nil     10 +)

         ("-a"         #*01    10 |-a|)
         ("-."         nil     10 -.)
         ("-:"         nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("-a"         nil     10 -a)

         (".a"         #*01    10 |.a|)
         (".:"         nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".a"         nil     10 .a)

         ("-.a"        #*001   10 |-.a|)
         ("-.:"        nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("-.a"        nil     10 -.a)

         ("1a"         nil     10 1a)
         ("1a"         #*01    10 |1a|)
         ("1:"         nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1.a"        #*001   10 |1.a|)
         ("1.:"        nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1.a"        nil     10 1.a)

         ("1/"         nil     10 1/)
         ("1/a"        #*001   10 |1/a|)
         ("1/:"        nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1/a"        nil     10 1/a)
         ("1/2a"       #*0001  10 |1/2a|)
         ("1/2:"       nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1/2a"       nil     10 1/2a)

         (".1a"        #*001   10 |.1a|)
         (".1:"        nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".1a"        nil     10 .1a)

         (".1e"        nil     10 .1e)
         (".1ea"       #*0001  10 |.1Ea|)
         (".1e:"       nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".1ea"       nil     10 .1ea)
         (".1e-"       nil     10 .1e-)
         (".1e+"       nil     10 .1e+)
         (".1e-a"      #*00001 10 |.1E-a|)
         (".1e-:"      nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".1e-a"      nil     10 .1e-a)
         (".1e2a"      #*00001 10 |.1E2a|)
         (".1e2:"      nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".1e2a"      nil     10 .1e2a)

         ("1aa"        #*001   16 |1Aa|)
         ("1a/"        nil     16 1a/)
         ("1a:"        nil     16 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1ax"        nil     16 1ax)

         ("aa"         #*01    10 |Aa|)
         ("a:"         nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("a::"        nil     10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("a:::"       nil     10 eclector.reader:symbol-can-have-at-most-two-package-markers)
         ("a:a:"       nil     10 eclector.reader:two-package-markers-must-be-adjacent)
         ("::a"        nil     10 eclector.reader:two-package-markers-must-not-be-first)
         ("keyword:b"  nil     10 :b)
         ("keyword::b" nil     10 :b)

         ;; decimal-integer
         ("1"          nil     10    1)
         ("-1"         nil     10   -1)
         ("-12"        nil     10  -12)

         ("1."         nil     10    1)

         ;; ratio
         ("+1/2"       nil     10    1/2)
         ("-1/2"       nil     10   -1/2)
         ("1/2"        nil     10    1/2)
         ("1/23"       nil     10    1/23)

         ;; float-no-exponent
         ("+.234"      nil     10     .234)
         ("-.234"      nil     10    -.234)
         (".234"       nil     10     .234)
         ("+1.234"     nil     10    1.234)
         ("-1.234"     nil     10   -1.234)
         ("1.234"      nil     10    1.234)

         ;; float-exponent
         ("+1.0e2"     nil     10  100.0)
         ("+1.0e-2"    nil     10    0.01)
         ("-1.0e2"     nil     10 -100.0)
         ("-1.0e-2"    nil     10   -0.01)
         ("1.0e2"      nil     10  100.0)
         ("1.0e-2"     nil     10    0.01)
         ("1.e2"       nil     10  100.0)
         ("1e2"        nil     10  100.0)
         ("1e01"       nil     10   10.0)

         ;; Nondefault *READ-BASE*
         ("a"          nil     16   10)
         ("-a"         nil     16  -10)
         ("1a"         nil     16   26)
         ("1aa"        nil     16  426))))
