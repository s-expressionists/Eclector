(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.tokens
    :in :eclector.reader)

(test interpret-token.default.smoke
  "Smoke test for default method on INTERPRET-TOKEN."

  (map nil (lambda (arguments-context-expected)
             (destructuring-bind
                   (token token-escapes *read-base* expected)
                 arguments-context-expected
               (let ((token-escapes (if token-escapes
                                        (map 'vector #'plusp token-escapes)
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
                      (is (equal expected (do-it)))))))))
       '(;; empty
         (""           nil   10 ||)
         ;; "consing dot"
         ("."          nil   10 eclector.reader:invalid-context-for-consing-dot)

         ;; symbol
         ("a"          #*1   10 |a|)
         ("-"          nil   10 -)
         ("+"          nil   10 +)

         ("-a"         #*01  10 |-a|)
         ("-."         nil   10 -.)
         ("-:"         nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("-a"         nil   10 -a)

         (".a"         #*01  10 |.a|)
         (".:"         nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".a"         nil   10 .a)

         ("-.a"        #*001 10 |-.a|)
         ("-.:"        nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("-.a"        nil   10 -.a)

         ("1a"         nil   10 1a)
         ("1a"         #*01  10 |1a|)
         ("1:"         nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1.a"        #*001 10 |1.a|)
         ("1.:"        nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1.a"        nil   10 1.a)

         ("1aa"        #*001 16 |1Aa|)
         ("1a/"        nil   16 1a/)
         ("1a:"        nil   16 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("1ax"        nil   16 1ax)

         ("aa"         #*01  10 |Aa|)
         ("a:"         nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("a::"        nil   10 eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("a:::"       nil   10 eclector.reader:symbol-can-have-at-most-two-package-markers)
         ("a:a:"       nil   10 eclector.reader:two-package-markers-must-be-adjacent)
         ("::a"        nil   10 eclector.reader:two-package-markers-must-not-be-first)
         ("keyword:b"  nil   10 :b)
         ("keyword::b" nil   10 :b)

         ;; decimal-integer
         ("1"          nil   10    1)
         ("-1"         nil   10   -1)
         ("-12"        nil   10  -12)

         ("1."         nil   10    1)

         ;; ratio
         ("+1/2"       nil   10    1/2)
         ("-1/2"       nil   10   -1/2)
         ("1/2"        nil   10    1/2)

         ;; float-no-exponent
         ("+.234"      nil   10     .234)
         ("-.234"      nil   10    -.234)
         (".234"       nil   10     .234)
         ("+1.234"     nil   10    1.234)
         ("-1.234"     nil   10   -1.234)
         ("1.234"      nil   10    1.234)

         ;; float-exponent
         ("+1.0e2"     nil   10  100.0)
         ("+1.0e-2"    nil   10    0.01)
         ("-1.0e2"     nil   10 -100.0)
         ("-1.0e-2"    nil   10   -0.01)
         ("1.0e2"      nil   10  100.0)
         ("1.0e-2"     nil   10    0.01)
         ("1.e2"       nil   10  100.0)
         ("1e2"        nil   10  100.0)

         ;; Nondefault *READ-BASE*
         ("a"          nil   16   10)
         ("-a"         nil   16  -10)
         ("1a"         nil   16   26)
         ("1aa"        nil   16  426))))
