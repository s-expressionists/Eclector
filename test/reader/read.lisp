(cl:in-package #:eclector.reader.test)

(in-suite* :eclector.reader.read
  :in :eclector.reader)

(test read-char/smoke
  "Smoke test for the READ-CHAR function."

  (mapc (lambda (input-args-expected)
          (destructuring-bind (input args expected) input-args-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (apply #'eclector.reader:read-char stream args))))
              (case expected
                (eclector.reader:end-of-file
                 (signals-printable eclector.reader:end-of-file (do-it)))
                (t
                 (is (equal expected (do-it))))))))

        '((""  ()         eclector.reader:end-of-file)
          (""  (nil)      nil)
          (""  (nil :eof) :eof)

          ("a" ()         #\a))))

(test peek-char/smoke
  "Smoke test for the PEEK-CHAR function."

  (mapc (lambda (input-args-expected)
          (destructuring-bind (input (peek-type &rest args) expected)
              input-args-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (apply #'eclector.reader:peek-char peek-type stream
                              args)))
                   (do-it/host ()
                     (with-input-from-string (stream input)
                       (apply #'cl:peek-char peek-type stream args))))
              (case expected
                (eclector.reader:end-of-file
                 (signals-printable eclector.reader:end-of-file (do-it))
                 (signals-printable end-of-file (do-it/host)))
                (t
                 (is (equal expected (do-it)))
                 (is (equal expected (do-it/host))))))))

        '(;; Peek type T
          (""   (t)            eclector.reader:end-of-file)
          (""   (t nil)        nil)
          (""   (t nil :eof)   :eof)

          (" "  (t)            eclector.reader:end-of-file)
          (" "  (t nil)        nil)
          (" "  (t nil :eof)   :eof)

          (" a" (t)            #\a)
          (" a" (t nil)        #\a)
          (" a" (t nil :eof)   #\a)

          ;; Peek type NIL
          (""   (nil)          eclector.reader:end-of-file)
          (""   (nil nil)      nil)
          (""   (nil nil :eof) :eof)

          (" "  (nil)          #\Space)
          (" "  (nil nil)      #\Space)
          (" "  (nil nil :eof) #\Space)

          (" a" (nil)          #\Space)
          (" a" (nil nil)      #\Space)
          (" a" (nil nil :eof) #\Space)

          ;; Peek type CHAR
          (""   (#\a)          eclector.reader:end-of-file)
          (""   (#\a nil)      nil)
          (""   (#\a nil :eof) :eof)

          (" "  (#\a)          eclector.reader:end-of-file)
          (" "  (#\a nil)      nil)
          (" "  (#\a nil :eof) :eof)

          (" a" (#\a)          #\a)
          (" a" (#\a nil)      #\a)
          (" a" (#\a nil :eof) #\a))))

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
                 (signals-printable eclector.reader:invalid-context-for-backquote
                   (do-it)))
                (eclector.reader:comma-not-inside-backquote
                 (signals-printable eclector.reader:comma-not-inside-backquote
                   (do-it)))
                (eclector.reader:object-must-follow-comma
                 (signals-printable eclector.reader:object-must-follow-comma
                   (do-it)))
                (eclector.reader:unknown-macro-sub-character
                 (signals-printable eclector.reader:unknown-macro-sub-character
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

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."

  (mapc (lambda (input-args-expected)
          (destructuring-bind
              (input args expected-value &optional expected-position)
              input-args-expected
            (flet ((do-it ()
                     (apply #'eclector.reader:read-from-string input args)))
              (case expected-value
                (eclector.reader:end-of-file
                 (signals eclector.reader:end-of-file (do-it)))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equal expected-value    value))
                   (is (eql   expected-position position))))))))
        '((""         ()                             eclector.reader:end-of-file)
          (""         (nil :eof)                     :eof                         0)

          (":foo 1 2" ()                             :foo                         5)
          (":foo 1 2" (t nil :preserve-whitespace t) :foo                         4)
          (":foo 1 2" (t nil :start 4)               1                            7)
          (":foo 1 2" (t nil :end 3)                 :fo                          3))))
