(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.read
  :in :eclector.reader)

(test read-char/smoke
  "Smoke test for the READ-CHAR function."
  (do-stream-input-cases ((length) args
                          expected &optional (error-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*standard-input* stream))
                 (apply #'eclector.reader:read-char
                        (substitute stream :stream args))))))
      (error-case (expected error-position)
        (error (do-it))
        (t (expect "result" (equal expected (do-it))))))
    '((""  ()                 eclector.reader:end-of-file)
      (""  (:stream)          eclector.reader:end-of-file)
      (""  (:stream nil)      nil)
      (""  (:stream nil :eof) :eof)

      ("a" (:stream)          #\a))))

(test peek-char/smoke
  "Smoke test for the PEEK-CHAR function."
  (do-stream-input-cases ((length) args
                          expected &optional (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*standard-input* stream))
                 (apply #'eclector.reader:peek-char
                        (substitute stream :stream args)))))
           (do-it/host ()
             (with-stream (stream)
               (let ((*standard-input* stream))
                 (apply #'cl:peek-char (substitute stream :stream args))))))
      (case expected
        (eclector.reader:end-of-file
         (signals-printable eclector.reader:end-of-file expected-position
           (do-it))
         (signals-printable end-of-file nil
           (do-it/host)))
        (t
         (expect "value"      (equal expected (do-it)))
         (expect "host value" (equal expected (do-it/host))))))
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
  (do-stream-input-cases ((length) read-suppress
                          expected &optional (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.reader:read stream)))))
      (error-case (expected expected-position)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    `(("(cons 1 2)"                 nil (cons 1 2))

      ("#+(or) `1 2"                nil 2)
      ("#+(or) #.(error \"foo\") 2" nil 2)
      ;; Some context-sensitive cases.
      ("#C(1 `,2)"                  nil eclector.reader:backquote-in-invalid-context 5)
      ("#C(#.`,(+ 1 2) 2)"          nil #C(3 2))
      ("#+`,common-lisp 1"          nil eclector.reader:backquote-in-invalid-context 2)
      ("#+#.`,:common-lisp 1"       nil 1)
      (",foo"                       nil eclector.reader:unquote-not-inside-backquote 0)
      (",@foo"                      nil eclector.reader:unquote-not-inside-backquote 0)
      ("`(,)"                       nil eclector.reader:object-must-follow-unquote 3)
      ("`(,@)"                      nil eclector.reader:object-must-follow-unquote 4)
      ("`(,.)"                      nil eclector.reader:object-must-follow-unquote 4)
      ("#1=`(,2)"                   nil (eclector.reader:quasiquote ((eclector.reader:unquote 2))))
      ;; Consing dot
      ("(1 . 2)"                    nil (1 . 2))
      ("(1 .||)"                    nil (1 |.|))
      ("(1 .|| 2)"                  nil (1 |.| 2))
      ("(1 #(.))"                   nil eclector.reader:invalid-context-for-consing-dot 5)
      ;; Interaction between *READ-SUPPRESS* and reader macros.
      ("#+(or) #|skipme|# 1 2"      nil 2)
      ("#+(or) ; skipme~%1 2"       nil 2)
      ;; Invalid macro sub-character.
      (,(format nil "#~C" #\Tab)    nil eclector.reader:sharpsign-invalid 1)
      (,(format nil "#~C" #\Tab)    t   eclector.reader:sharpsign-invalid 1)
      ("#<"                         nil eclector.reader:sharpsign-invalid 1)
      ("#<"                         t   eclector.reader:sharpsign-invalid 1)
      ;; Unknown macro sub-character.
      ("#!"                         nil eclector.reader:unknown-macro-sub-character 1)
      ("#!"                         t   eclector.reader:unknown-macro-sub-character 1)
      ;; End of input while trying to read macro sub character.
      ("#"                          nil eclector.reader:unterminated-dispatch-macro 1)
      ("#"                          t   eclector.reader:unterminated-dispatch-macro 1))))

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

(test read/runtime-recursive-p
  "Test READ with RECURSIVE-P being unknown until runtime."
  (do-stream-input-cases ((length) recursive-p
                          expected-result &optional (expected-position length))
      (multiple-value-bind (result position)
          (with-stream (stream)
            (eclector.reader:read stream nil :eof recursive-p))
        (expect "result"   (eql expected-result   result))
        (expect "position" (eql expected-position position)))
    '(("1" nil 1)
      ("1" t   1))))

(test read-preserving-whitespace/smoke
  "Smoke test for the READ-PRESERVING-WHITESPACE function."
  (do-stream-input-cases ((length) eof-error-p eof-value
                          expected-result &optional (expected-position length))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader:read-preserving-whitespace
                stream eof-error-p eof-value))))
      (error-case (expected-result expected-position)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected-result   result))
             (expect "position" (equal expected-position position))))))
    '(;; End of input situations
      (""        t   nil  eclector.reader:end-of-file)
      (""        nil :eof :eof)
      ;; Valid
      (":foo"    t   nil  :foo)
      (":foo "   t   nil  :foo                        4)
      (":foo  "  t   nil  :foo                        4)
      (":foo  1" t   nil  :foo                        4)
      ("#*11 "   t   nil  #*11                        4)
      ("#1*1 "   t   nil  #*1                         4))))

(test read-from-string/smoke
  "Smoke test for the READ-FROM-STRING function."
  (do-input-cases (input args expected-value &optional expected-position)
      (flet ((do-it ()
               (apply #'eclector.reader:read-from-string input args)))
        (error-case (expected-value expected-position)
          (error (do-it))
          (t (multiple-value-bind (value position) (do-it)
               (expect "value"    (equal expected-value    value))
               (expect "position" (eql   expected-position position))))))
    '(;; End of input situations
      (""         ()                               eclector.reader:end-of-file 0)
      (""         (nil :eof)                       :eof                        0)
      ;; Valid
      (":foo 1 2" ()                               :foo                        5)
      ;; Start and end
      (":foo 1 2" (t nil :start 4)                 1                           7)
      (":foo 1 2" (t nil :end 3)                   :fo                         3)
      ;; Preserving whitespace
      (":foo 1"   (t nil :preserve-whitespace nil) :foo                        5)
      (":foo 1 "  (t nil :preserve-whitespace nil) :foo                        5)
      (":foo 1  " (t nil :preserve-whitespace nil) :foo                        5)
      (":foo 1 2" (t nil :preserve-whitespace nil) :foo                        5)
      ("#*11 "    (t nil :preserve-whitespace nil) #*11                        5)
      ("#1*1 "    (t nil :preserve-whitespace nil) #1*1                        5)

      (":foo 1"   (t nil :preserve-whitespace t)   :foo                        4)
      (":foo 1 "  (t nil :preserve-whitespace t)   :foo                        4)
      (":foo 1  " (t nil :preserve-whitespace t)   :foo                        4)
      (":foo 1 2" (t nil :preserve-whitespace t)   :foo                        4)
      ("#*11 "    (t nil :preserve-whitespace t)   #*11                        4)
      ("#1*1 "    (t nil :preserve-whitespace t)   #1*1                        4))))

(test read-maybe-nothing/smoke
  "Smoke test for the READ-MAYBE-NOTHING function."
  (do-stream-input-cases ((length) (eof-error-p read-suppress)
                          expected-value
                          &optional expected-kind (expected-position length))
      (flet ((do-it ()
               (with-stream (stream)
                 (eclector.reader:call-as-top-level-read
                  nil (lambda ()
                        (let ((*read-suppress* read-suppress))
                          (eclector.reader:read-maybe-nothing
                           nil stream eof-error-p :eof)))
                  stream eof-error-p :eof t))))
        (error-case (expected-value expected-position)
          (error (do-it))
          (t (multiple-value-bind (value kind position) (do-it)
               (expect "value"    (equal expected-value    value))
               (expect "kind"     (eq    expected-kind     kind))
               (expect "position" (eql   expected-position position))))))
    '(;; End of input situations
      (""       (nil nil) :eof :eof       0)
      (""       (t   nil) eclector.reader:end-of-file)
      ("."      (nil nil) eclector.reader:invalid-context-for-consing-dot nil 0)
      ;; Valid
      ("   "    (nil nil) nil :whitespace 3)
      ("   "    (nil nil) nil :whitespace 3)

      (";  "    (nil nil) nil :skip       3)

      ("#||#"   (nil nil) nil :skip       4)
      ("#||# "  (nil nil) nil :skip       4)
      ("#||#  " (nil nil) nil :skip       4)
      ("#||#"   (nil t)   nil :skip       4)

      ("1"      (nil nil) 1   :object     1)
      ("1 "     (nil nil) 1   :object     1)
      ("1"      (nil t)   nil :suppress   1)
      ("1 "     (nil t)   nil :suppress   1)
      ("."      (nil t)   nil :suppress   1)
      (". "     (nil t)   nil :suppress   1))))

(test read-delimited-list/smoke
  "Smoke test for the READ-DELIMITED-LIST function."
  (do-stream-input-cases ((length) char expected1
                          &optional (expected2 expected1)
                                    (expected-position length))
    (flet ((do-it (install-macro-p)
             (let ((readtable (eclector.readtable:copy-readtable
                               eclector.reader:*readtable*)))
               (when install-macro-p
                 (eclector.readtable:set-macro-character
                  readtable #\]
                  (eclector.readtable:get-macro-character readtable #\))))
               (let ((eclector.reader:*readtable* readtable))
                 (with-stream (stream)
                   (eclector.reader:read-delimited-list char stream nil))))))
      ;; Test with #\] behaving like #\).
      (error-case (expected1 expected-position)
        (error (do-it t))
        (t (expect "result1" (relaxed-equalp expected1 (do-it t)))))
      ;; Test with #\] having constituent syntax type.
      (error-case (expected2 expected-position)
        (error (do-it nil))
        (t (expect "result2" (relaxed-equalp expected2 (do-it nil))))))
    '((""             #\] eclector.reader:unterminated-list)
      (")"            #\] eclector.reader:invalid-context-for-right-parenthesis
                          eclector.reader:invalid-context-for-right-parenthesis
                          0)
      ("]"            #\] ())
      ("1"            #\] eclector.reader:unterminated-list)
      ("."            #\] eclector.reader:invalid-context-for-consing-dot
                          eclector.reader:invalid-context-for-consing-dot
                          0)
      ("1]"           #\] (1) eclector.reader:unterminated-list)
      ("1 ]"          #\] (1))
      ("1 #|2|# ]"    #\] (1))
      ("1 #+(or) 2 ]" #\] (1))
      ("1 #()]"       #\] (1 #()))
      ("1 #(.)]"      #\] eclector.reader:invalid-context-for-consing-dot
                          eclector.reader:invalid-context-for-consing-dot
                          4)
      ;; We call READ-DELIMITED-LIST with RECURSIVE-P being false, so
      ;; labels and references should work between and within list
      ;; elements.
      ("#1=1 #1#]"    #\] (1 1))
      ("(#1=1 #1#)]"  #\] ((1 1))))))

(test read-delimited-list/runtime-recursive-p
  "Test READ-DELIMITED-LIST with RECURSIVE-P being unknown until runtime."
  (do-stream-input-cases ((length) recursive-p
                          expected-result &optional (expected-position length))
      (multiple-value-bind (result position)
          (with-stream (stream)
            (eclector.reader:read-delimited-list #\) stream recursive-p))
        (expect "result"   (equal expected-result   result))
        (expect "position" (eql   expected-position position)))
      '(("1 2)" nil (1 2))
        ("1 2)" t   (1 2)))))
