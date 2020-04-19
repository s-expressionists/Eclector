(cl:in-package #:eclector.reader.test)

(defun relaxed-equalp (expected value)
  "Examples:

  (relaxed-equalp \"foo\" \"FOO\") => nil

  (relaxed-equalp '#:foo '#:foo) => t

  (relaxed-equalp #(1 2) #(1 2)) => t

  (relaxed-equalp '(foo #:bar 1) '(foo #:bar 1)) => t"
  (labels ((mappablep (thing)
             (and (typep thing 'sequence)
                  (or (not (listp thing))
                      (alexandria:proper-list-p thing))))
           (rec (expected value)
             (cond ((stringp expected)
                    (equal expected value))
                   ((mappablep expected)
                    (and (eq (class-of expected) (class-of value))
                         (= (length expected) (length value))
                         (every #'relaxed-equalp expected value)))
                   ((not (symbolp expected))
                    (equalp expected value))
                   ((not (symbol-package expected))
                    (string= (symbol-name expected)
                             (symbol-name value)))
                   (t
                    (eq expected value)))))
    (rec expected value)))

;;; Client for testing SHARPSIGN-S

(defclass sharpsign-s-client () ())

(defmethod eclector.reader:make-structure-instance
    ((client sharpsign-s-client) (name t) (initargs t))
  (list* name initargs))
