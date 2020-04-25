(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.read
  :in :eclector.reader)

(test read-char/smoke
  "Smoke test for the READ-CHAR function."

  (mapc (lambda (input-args-expected)
          (destructuring-bind (input args expected) input-args-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*standard-input* stream))
                         (apply #'eclector.reader:read-char
                                (substitute stream :stream args))))))
              (error-case expected
                (error (do-it))
                (t
                 (is (equal expected (do-it))))))))

        '((""  ()                 eclector.reader:end-of-file)
          (""  (:stream)          eclector.reader:end-of-file)
          (""  (:stream nil)      nil)
          (""  (:stream nil :eof) :eof)

          ("a" (:stream)          #\a))))

(test peek-char/smoke
  "Smoke test for the PEEK-CHAR function."

  (mapc (lambda (input-args-expected)
          (destructuring-bind (input args expected)
              input-args-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (let ((*standard-input* stream))
                         (apply #'eclector.reader:peek-char
                                (substitute stream :stream args)))))
                   (do-it/host ()
                     (with-input-from-string (stream input)
                       (let ((*standard-input* stream))
                         (apply #'cl:peek-char
                                (substitute stream :stream args))))))
              (case expected
                (eclector.reader:end-of-file
                 (signals-printable eclector.reader:end-of-file (do-it))
                 (signals-printable end-of-file (do-it/host)))
                (t
                 (is (equal expected (do-it)))
                 (is (equal expected (do-it/host))))))))

        '(;; Peek type T
          (""   (t :stream)            eclector.reader:end-of-file)
          (""   (t :stream nil)        nil)
          (""   (t :stream nil :eof)   :eof)

          (" "  (t :stream)            eclector.reader:end-of-file)
          (" "  (t :stream nil)        nil)
          (" "  (t :stream nil :eof)   :eof)

          (" a" (t :stream)            #\a)
          (" a" (t :stream nil)        #\a)
          (" a" (t :stream nil :eof)   #\a)

          ;; Peek type NIL
          (""   ()                     eclector.reader:end-of-file)
          (""   (nil :stream)          eclector.reader:end-of-file)
          (""   (nil :stream nil)      nil)
          (""   (nil :stream nil :eof) :eof)

          (" "  (nil :stream)          #\Space)
          (" "  (nil :stream nil)      #\Space)
          (" "  (nil :stream nil :eof) #\Space)

          (" a" (nil :stream)          #\Space)
          (" a" (nil :stream nil)      #\Space)
          (" a" (nil :stream nil :eof) #\Space)

          ;; Peek type CHAR
          (""   (#\a :stream)          eclector.reader:end-of-file)
          (""   (#\a :stream nil)      nil)
          (""   (#\a :stream nil :eof) :eof)

          (" "  (#\a :stream)          eclector.reader:end-of-file)
          (" "  (#\a :stream nil)      nil)
          (" "  (#\a :stream nil :eof) :eof)

          (" a" (#\a :stream)          #\a)
          (" a" (#\a :stream nil)      #\a)
          (" a" (#\a :stream nil :eof) #\a))))

(test read/smoke
  "Smoke test for the READ function."

  ;; This test focuses on interactions between different parts of the
  ;; reader since the individual parts in isolation are handled by
  ;; more specific tests.
  (mapc (lambda (input-and-expected)
          (destructuring-bind (input read-suppress expected) input-and-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (let ((*read-suppress* read-suppress))
                                 (eclector.reader:read stream))
                               (file-position stream)))))
              (error-case expected
                (error (do-it))
                (t
                 (multiple-value-bind (result position) (do-it)
                   (is (equal expected       result))
                   (is (eql   (length input) position))))))))

        `(("(cons 1 2)"                 nil (cons 1 2))

          ("#+(or) `1 2"                nil 2)
          ("#+(or) #.(error \"foo\") 2" nil 2)

          ;; Some context-sensitive cases.
          ("#C(1 `,2)"                  nil eclector.reader:backquote-in-invalid-context)
          ("#C(#.`,(+ 1 2) 2)"          nil #C(3 2))
          ("#+`,common-lisp 1"          nil eclector.reader:backquote-in-invalid-context)
          ("#+#.`,:common-lisp 1"       nil 1)
          (",foo"                       nil eclector.reader:unquote-not-inside-backquote)
          (",@foo"                      nil eclector.reader:unquote-not-inside-backquote)
          ("`(,)"                       nil eclector.reader:object-must-follow-unquote)
          ("`(,@)"                      nil eclector.reader:object-must-follow-unquote)
          ("`(,.)"                      nil eclector.reader:object-must-follow-unquote)
          ("#1=`(,2)"                   nil (eclector.reader:quasiquote ((eclector.reader:unquote 2))))

          ;; Consing dot
          ("(1 . 2)"                    nil (1 . 2))
          ("(1 .||)"                    nil (1 |.|))
          ("(1 .|| 2)"                  nil (1 |.| 2))

          ;; Interaction between *READ-SUPPRESS* and reader macros.
          ("#+(or) #|skipme|# 1 2"      nil 2)
          ("#+(or) ; skipme
            1 2"                        nil 2)

          ;; Invalid macro sub-character.
          (,(format nil "#~C" #\Tab)    nil eclector.reader:sharpsign-invalid)
          (,(format nil "#~C" #\Tab)    t   eclector.reader:sharpsign-invalid)
          ("#<"                         nil eclector.reader:sharpsign-invalid)
          ("#<"                         t   eclector.reader:sharpsign-invalid)
          ;; Unknown macro sub-character.
          ("#!"                         nil eclector.reader:unknown-macro-sub-character)
          ("#!"                         t   eclector.reader:unknown-macro-sub-character)
          ;; End of input while trying to read macro sub character.
          ("#"                          nil eclector.reader:unterminated-dispatch-macro)
          ("#"                          t   eclector.reader:unterminated-dispatch-macro))))

(defclass unbound-slot-class ()
  ((%unbound-slot)))

(test read/circularity-and-standard-objects
  "Test the combination of circularity and standard instance literals."

  (let* ((input "(#1=#.(make-instance 'unbound-slot-class) #1#)")
         (result (read-from-string input)))
    (is-true (typep result '(cons unbound-slot-class
                                  (cons unbound-slot-class null))))
    (destructuring-bind (first second) result
      (is (eq first second))
      (is-false (slot-boundp first '%unbound-slot)))))

(test read-preserving-whitespace/smoke
  "Smoke test for the READ-PRESERVING-WHITESPACE function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input eof-error-p eof-value
                               expected-result &optional expected-position)
              input-and-expected
            (flet ((do-it ()
                     (with-input-from-string (stream input)
                       (values (eclector.reader:read-preserving-whitespace
                                stream eof-error-p eof-value)
                               (file-position stream)))))
              (error-case expected-result
                (error (do-it))
                (t
                 (is (equal (values expected-result expected-position)
                            (do-it))))))))

        '((""        t   nil  eclector.reader:end-of-file)
          (""        nil :eof :eof                        0)

          (":foo"    t   nil  :foo                        4)
          (":foo "   t   nil  :foo                        4)
          (":foo  "  t   nil  :foo                        4)
          (":foo  1" t   nil  :foo                        4))))

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."

  (mapc (lambda (input-args-expected)
          (destructuring-bind
              (input args expected-value &optional expected-position)
              input-args-expected
            (flet ((do-it ()
                     (apply #'eclector.reader:read-from-string input args)))
              (error-case expected-value
                (error (do-it))
                (t
                 (multiple-value-bind (value position) (do-it)
                   (is (equal expected-value    value))
                   (is (eql   expected-position position))))))))
        '((""         ()                               eclector.reader:end-of-file)
          (""         (nil :eof)                       :eof                         0)

          (":foo 1 2" ()                               :foo                         5)

          ;; Start and end
          (":foo 1 2" (t nil :start 4)                 1                            7)
          (":foo 1 2" (t nil :end 3)                   :fo                          3)

          ;; Preserving whitespace
          (":foo 1"   (t nil :preserve-whitespace nil) :foo                         5)
          (":foo 1 "  (t nil :preserve-whitespace nil) :foo                         5)
          (":foo 1  " (t nil :preserve-whitespace nil) :foo                         5)
          (":foo 1 2" (t nil :preserve-whitespace nil) :foo                         5)

          (":foo 1"   (t nil :preserve-whitespace t)   :foo                         4)
          (":foo 1 "  (t nil :preserve-whitespace t)   :foo                         4)
          (":foo 1  " (t nil :preserve-whitespace t)   :foo                         4)
          (":foo 1 2" (t nil :preserve-whitespace t)   :foo                         4))))

(test read-delimited-list/smoke
  "Smoke test for the READ-DELIMITED-LIST function."

  (mapc (lambda (input-char-expected)
          (destructuring-bind (input char expected) input-char-expected
            (flet ((do-it ()
                     (let ((readtable (eclector.readtable:copy-readtable
                                       eclector.reader:*readtable*)))
                       (eclector.readtable:set-macro-character
                        readtable #\]
                        (eclector.readtable:get-macro-character readtable #\)))
                       (let ((eclector.reader:*readtable* readtable))
                         (with-input-from-string (stream input)
                           (eclector.reader:read-delimited-list
                            char stream nil))))))
              (error-case expected
                (error (do-it))
                (t
                 (is (equal expected (do-it))))))))
        '((""    #\] eclector.reader:unterminated-list)
          (")"   #\] eclector.reader:invalid-context-for-right-parenthesis)
          ("]"   #\] ())
          ("1"   #\] eclector.reader:unterminated-list)
          ("."   #\] eclector.reader:invalid-context-for-consing-dot)
          ("1]"  #\] (1))
          ("1 ]" #\] (1)))))
