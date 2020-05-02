(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.readtable-interaction
  :in :eclector)

(test read/readtable-interaction
  "Test for the interaction between READ and the readtable."

  (mapc
   (lambda (setup-cases)
     (destructuring-bind (setup &rest cases) setup-cases
       (mapc (lambda (input-expected)
               (destructuring-bind (input expected
                                    &optional (expected-position (length input)))
                   input-expected
                 (flet ((do-it ()
                          (let ((eclector.reader:*readtable*
                                  (funcall setup (eclector.readtable:copy-readtable
                                                  eclector.reader:*readtable*))))
                            (with-input-from-string (stream input)
                              (values (eclector.reader:read stream)
                                      (file-position stream))))))
                   (error-case expected
                     (error (do-it))
                     (t
                      (multiple-value-bind (result position) (do-it)
                        (is (equal expected          result))
                        (is (eql   expected-position position))))))))
             cases)))
   `(;; Change syntax of a character
     (,(lambda (readtable)
         (setf (eclector.readtable:syntax-from-char #\7 readtable readtable) #\;)
         readtable)
      ("123456789" 123456 6))
     ;; And using the standard function
     (,(lambda (readtable)
         (eclector.readtable:set-syntax-from-char #\7 #\; readtable readtable)
         readtable)
      ("123456789" 123456 6))

     ;; Change syntax from whitespace to (invalid) constituent
     (,(lambda (readtable)
         (setf (eclector.readtable:syntax-from-char #\Space readtable readtable) #\A)
         readtable)
      (" "  eclector.reader:invalid-constituent-character)
      ("b " eclector.reader:invalid-constituent-character))

     ;; Change [ and ] to the syntax of ( and )
     (,(lambda (readtable)
         (eclector.readtable:set-macro-character
          readtable #\] (eclector.readtable:get-macro-character readtable #\)))
         (eclector.readtable:set-macro-character
          readtable #\[ (lambda (stream char)
                          (declare (ignore char))
                          (eclector.reader:read-delimited-list #\] stream t)))
         readtable)
      ("(1 2)"   (1 2))
      ("[1 2]"   (1 2))
      ("#C(1 2)" #C(1 2))
      ("#C[1 2]" #C(1 2))))))

(test peek-char/readtable-interaction
  "Test for the interaction between PEEK-CHAR and the readtable."

  (mapc
   (lambda (setup-input-expected)
     (destructuring-bind
         (setup input expected &optional (expected-position (length input)))
         setup-input-expected
       (flet ((do-it ()
                (let ((eclector.reader:*readtable*
                        (funcall setup (eclector.readtable:copy-readtable
                                        eclector.reader:*readtable*))))
                  (with-input-from-string (stream input)
                    (values (eclector.reader:peek-char t stream)
                            (file-position stream))))))
         (case expected
           (t
            (multiple-value-bind (result position) (do-it)
              (is (equal expected          result))
              (is (eql   expected-position position))))))))

   `(;; Default
     (,#'identity
      " x" #\x 1)
     ;; Change syntax of a #\Space
     (,(lambda (readtable)
         (setf (eclector.readtable:syntax-from-char #\Space readtable readtable)
               :single-escape)
         readtable)
      " x" #\Space 0))))
