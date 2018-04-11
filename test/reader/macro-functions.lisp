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

(test sharpsign-dot/smoke
  "Smoke test for the SHARPSIGN-DOT reader macro function."

  (mapc (lambda (input-parameters-expected)
          (destructuring-bind (input parameter read-suppress expected)
              input-parameters-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((*read-suppress* read-suppress))
                                 (eclector.reader::sharpsign-dot
                                  stream #\. parameter))
                               (file-position stream)))))
              (case expected
                (eclector.reader:numeric-parameter-supplied-but-ignored
                 (signals eclector.reader:numeric-parameter-supplied-but-ignored
                   (do-it)))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected       result))
                   (is (eql   (length input) position))))))))
        '(;; Error cases
          ("1"               1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ;; Good inputs
          ("1"               nil nil 1)
          ("(+ 1 2)"         nil nil 3)

          ("`1"              nil nil 1)

          ;; Interaction with *READ-SUPPRESS*.
          ("(error \"foo\")" nil t   nil))))

(test read-rational/smoke
  "Smoke test for the READ-RATIONAL reader macro function."

  (mapc (lambda (input-base-preserve-expected)
          (destructuring-bind
                (input base eclector.reader:*preserve-whitespace* expected)
              input-base-preserve-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (eclector.reader::read-rational stream base))))
              (case expected
                (end-of-file
                 (signals end-of-file (do-it)))
                (eclector.reader:digit-expected
                 (signals eclector.reader:digit-expected (do-it)))
                (t
                 (is (eql expected (do-it))))))))
        '(;; Error cases
          (" "      10 nil eclector.reader:digit-expected)
          ("x"      10 nil eclector.reader:digit-expected)
          ("- "     10 nil eclector.reader:digit-expected)
          ("-x"     10 nil eclector.reader:digit-expected)
          ("1x"     10 nil eclector.reader:digit-expected)
          ("1#"     10 t   eclector.reader:digit-expected)
          ("1/"     10 nil end-of-file)
          ("1/ "    10 nil eclector.reader:digit-expected)
          ("1/x"    10 nil eclector.reader:digit-expected)
          ("1/1x"   10 nil eclector.reader:digit-expected)
          ("1/1#"   10 nil eclector.reader:digit-expected)
          ;; Good inputs
          ("1"      10 nil 1)
          ("-1"     10 nil -1)
          ("1 "     10 nil 1)
          ("-1 "    10 nil -1)
          ("1 "     10 t   1)
          ("-1 "    10 t   -1)
          ("12"     10 nil 12)
          ("-12"    10 nil -12)
          ("12 "    10 nil 12)
          ("-12 "   10 nil -12)
          ("12 "    10 t   12)
          ("-12 "   10 t   -12)
          ("12("    10 nil 12)
          ("-12("   10 nil -12)
          ("1/2"    10 nil 1/2)
          ("-1/2"   10 nil -1/2)
          ("1/2 "   10 nil 1/2)
          ("-1/2 "  10 nil -1/2)
          ("1/2 "   10 t   1/2)
          ("-1/2 "  10 t   -1/2)
          ("1/2("   10 nil 1/2)
          ("-1/2("  10 nil -1/2)
          ("1/23"   10 nil 1/23)
          ("-1/23"  10 nil -1/23)
          ("1/23 "  10 nil 1/23)
          ("-1/23 " 10 nil -1/23)
          ("1/23 "  10 t   1/23)
          ("-1/23 " 10 t   -1/23)
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
              (case expected
                (eclector.reader:digit-expected
                 (signals eclector.reader:digit-expected (do-it)))
                (eclector.reader:no-elements-found
                 (signals eclector.reader:no-elements-found (do-it)))
                (eclector.reader:too-many-elements
                 (signals eclector.reader:too-many-elements (do-it)))
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
          ("1"   2   nil #2*1)
          ("1 "  2   nil #2*1)
          ("1)"  2   nil #2*1 1)
          ;; With *read-suppress* bound to t
          (""    nil t   nil)
          ("1"   nil t   nil)
          ("11"  2   t   nil)
          ("1"   2   t   nil)
          ("abc" 2   t   nil)
          ("abc" nil t   nil))))

(test sharpsign-p/smoke
  "Smoke test for the SHARPSIGN-P function."

  (mapc (lambda (input-parameter-read-suppress-expected)
          (destructuring-bind
                (input parameter read-suppress expected)
              input-parameter-read-suppress-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*read-suppress* read-suppress))
                         (values
                          (eclector.reader::sharpsign-p stream #\P parameter)
                          (file-position stream))))))
              (case expected
                (type-error
                 (signals type-error (do-it)))
                (eclector.reader:numeric-parameter-supplied-but-ignored
                 (signals eclector.reader:numeric-parameter-supplied-but-ignored
                   (do-it)))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equal expected value))
                   (is (equal (length input) position))))))))
        '(;; Errors
          ("1"       nil nil type-error)
          ("\"foo\"" 1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ;; Valid
          ("\"foo\"" nil nil #P"foo")
          ;; With *read-supress* bound to t
          ("\"foo\"" nil t   nil))))

(test sharpsign-plus-minus/smoke
  "Smoke test for the SHARPSIGN-{PLUS,MINUS} functions."

  (mapc (lambda (input-parameter-context-expected)
          (destructuring-bind
                (input parameter *read-suppress*
                       plus-expected &optional minus-expected)
              input-parameter-context-expected
            (flet ((do-it (which)
                     (with-input-from-string (stream input)
                       (values
                        (ecase which
                          (:plus  (eclector.reader::sharpsign-plus
                                   stream #\+ parameter))
                          (:minus (eclector.reader::sharpsign-minus
                                   stream #\- parameter)))
                        (file-position stream)))))
              (case plus-expected
                (type-error
                 (signals type-error (do-it :plus))
                 (signals type-error (do-it :minus)))
                (eclector.reader:single-feature-expected
                 (signals eclector.reader:single-feature-expected (do-it :plus))
                 (signals eclector.reader:single-feature-expected (do-it :minus)))
                (eclector.reader:numeric-parameter-supplied-but-ignored
                 (signals eclector.reader:numeric-parameter-supplied-but-ignored
                   (do-it :plus))
                 (signals eclector.reader:numeric-parameter-supplied-but-ignored
                   (do-it :minus)))
                (t
                 (multiple-value-bind (value position) (do-it :plus)
                   (is (equal plus-expected value))
                   (is (equal (length input) position)))
                 (multiple-value-bind (value position) (do-it :minus)
                   (is (equal minus-expected value))
                   (is (equal (length input) position))))))))
        '(;; Errors
          ("1"                     nil nil type-error)
          ("(1)"                   nil nil type-error)
          ("(not :foo :bar)"       nil nil eclector.reader:single-feature-expected)
          ("(not 1)"               nil nil type-error)
          ("(and 1)"               nil nil type-error)
          ("(or 1)"                nil nil type-error)
          ("(and) 1"               1   nil eclector.reader:numeric-parameter-supplied-but-ignored)
          ;; Valid
          ("common-lisp 1"         nil nil 1   nil)
          ("(not common-lisp) 1"   nil nil nil 1)
          ("(and) 1"               nil nil 1   nil)
          ("(or) 1"                nil nil nil 1)
          ("(not (not (and))) 1"   nil nil 1   nil)
          ;; With *read-supress* bound to t
          ("(and) 1"               nil t   nil nil)
          ;; In which package is the guarded expression read?
          ("(and) foo"             nil nil foo nil)
          ;; Vendor extension shouldn't break other implementations
          ;; when guarded properly
          ("(and some-lisp
                 (version> \"1\"))
            1"                     nil nil nil 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign equals and sharpsign sharpsign.

(test sharpsign-{equal\,sharpsign}/smoke
  "Smoke test for the SHARPSIGN-{EQUAL,SHARPSIGN} functions."

  (mapc (lambda (input-expected)
          (destructuring-bind (input expected) input-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (eclector.reader:read stream))))
              (case expected
                (eclector.reader:numeric-parameter-not-supplied-but-required
                 (signals eclector.reader:numeric-parameter-not-supplied-but-required
                   (do-it)))
                (eclector.reader:sharpsign-equals-label-defined-more-than-once
                 (signals eclector.reader:sharpsign-equals-label-defined-more-than-once
                   (do-it)))
                (eclector.reader:sharpsign-sharpsign-undefined-label
                 (signals eclector.reader:sharpsign-sharpsign-undefined-label
                   (do-it)))
                (recursive-cons
                 (let ((result (do-it)))
                   (is-true (consp result))
                   (is (eq result (car result)))))
                (t
                 (is (equalp expected (do-it))))))))
        '(;; sharpsign equals errors
          ("#="             eclector.reader:numeric-parameter-not-supplied-but-required)
          ("(#1=1 #1=2)"    eclector.reader:sharpsign-equals-label-defined-more-than-once)
          ;; sharpsign sharpsign errors
          ("##"             eclector.reader:numeric-parameter-not-supplied-but-required)
          ("#1#"            eclector.reader:sharpsign-sharpsign-undefined-label)
          ("(#1=1 #2#)"     eclector.reader:sharpsign-sharpsign-undefined-label)
          ;;
          ("(#1=1)"         (1))
          ("(#1=1 #1#)"     (1 1))
          ("(#1=1 #1# #1#)" (1 1 1))
          ("#1=(#1#)"       recursive-cons)
          ;; There was problem leading to unrelated expressions of the
          ;; forms (nil) and (t) being replaced by the fixup
          ;; processor.
          ("(#1=1 (nil))"   (1 (nil)))
          ("(#1=((nil)))"   (((nil))))
          ("(#1=1 (t))"     (1 (t)))
          ("(#1=((t)))"     (((t)))))))
