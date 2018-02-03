(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.tokens
    :in :eclector.reader)

(test interpret-token.default.smoke
  "Smoke test for default method on INTERPRET-TOKEN."

  (map nil (lambda (arguments-expected)
             (destructuring-bind (token token-escapes expected)
                 arguments-expected
               (let ((token-escapes (if token-escapes
                                        (map 'vector #'plusp token-escapes)
                                        (make-array (length token)
                                                    :initial-element nil))))
                 (flet ((do-it ()
                          (with-input-from-string (stream "")
                            (eclector.reader:interpret-token
                             token token-escapes stream))))
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
       ;; TODO *read-case*, *read-base* parameters
       '(;; empty
         (""           nil   ||)
         ;; "consing dot"
         ("."          nil   eclector.reader:invalid-context-for-consing-dot)

         ;; symbol
         ("a"          #*1   |a|)
         ("-"          nil   -)
         ("+"          nil   +)

         ("-a"         #*01  |-a|)
         ("-."         nil   -.)
         ("-:"         nil   eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("-a"         nil   -a)

         (".a"         #*01  |.a|)
         (".:"         nil   eclector.reader:symbol-name-must-not-end-with-package-marker)
         (".a"         nil   .a)

         ("-.a"        #*001 |-.a|)
         ("-.:"        nil   eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("-.a"        nil   -.a)

         ("aa"         #*01  |Aa|)
         ("a:"         nil   eclector.reader:symbol-name-must-not-end-with-package-marker)
         ; ("a::"        nil   eclector.reader:symbol-name-must-not-end-with-package-marker)
         ("a:::"       nil   eclector.reader:symbol-can-have-at-most-two-package-markers)
         ("a:a:"       nil   eclector.reader:two-package-markers-must-be-adjacent)
         ("::a"        nil   eclector.reader:two-package-markers-must-not-be-first)
         ("keyword:b"  nil   :b)
         ("keyword::b" nil   :b)

         ;; ratio
         ("+1/2"       nil      1/2)
         ("-1/2"       nil     -1/2)
         ("1/2"        nil      1/2)

         ;; float-no-exponent
         ("+.234"      nil       .234)
         ("-.234"      nil      -.234)
         (".234"       nil       .234)
         ("+1.234"     nil      1.234)
         ("-1.234"     nil     -1.234)
         ("1.234"      nil      1.234)

         ;; float-exponent
         ("+1.0e2"     nil    100.0)
         ("+1.0e-2"    nil      0.01)
         ("-1.0e2"     nil   -100.0)
         ("-1.0e-2"    nil     -0.01)
         ("1.0e2"      nil    100.0)
         ("1.0e-2"     nil      0.01))))
