(cl:defpackage #:eclector.tools-for-build.write-news
  (:use
   #:cl)

  (:import-from #:eclector.tools-for-build.read-changes
   #:punctuationp
   #:split-into-lines)

  (:export
   #:write-news))

(cl:in-package #:eclector.tools-for-build.write-news)

;;; Write plaintext NEWS

(defun write-paragraph (paragraph stream)
  (destructuring-bind (keyword &rest content) paragraph
    (assert (eq keyword :paragraph))
    (pprint-logical-block (stream content)
      (loop for (chunk next) on content
            do (etypecase chunk
                 ((cons (eql :when)))
                 ((cons (eql :symbol))
                  (format stream "~:@(~A~)" (second chunk)))
                 ((cons (eql :tt))
                  (write-string (second chunk) stream))
                 (string
                  (write-string chunk stream)))
            when (and next
                      (not (or (eq (punctuationp next) t)
                               (typep next '(cons (eql :when)))))
                      (not (eq (punctuationp chunk) :open)))
            do (write-string " " stream)
               (pprint-newline :fill stream)))))

(defun write-code (code stream)
  (destructuring-bind (keyword content) code
    (assert (eq keyword :code))
    (pprint-logical-block (stream code :per-line-prefix "  ")
      (let ((lines (split-into-lines content)))
        (loop for (line next) on lines
              do (write-string line stream)
              when next
                do (pprint-newline :mandatory stream))))))

(defun write-item (item stream)
  (destructuring-bind (keyword &rest children) item
    (assert (eq keyword :item))
    (format stream "* ")
    (pprint-logical-block (stream children)
      (loop for (child next) on children
            do (etypecase child
                 ((cons (eql :paragraph))
                  (write-paragraph child stream))
                 ((cons (eql :code))
                  (write-code child stream)))
               (when next
                 (format stream "~@:_~@:_"))))
    (format stream "~@:_~@:_")))

(defun write-release (release stream)
  (destructuring-bind (keyword version date &rest items) release
    (assert (eq keyword :release))
    (format stream "Release ~A (~:[not yet released~;~:*~A~])~@:_~
                    ~@:_"
            version date)
    (dolist (item items)
      (write-item item stream))))

(defun write-news (changes filename)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede)
    (pprint-logical-block (stream changes)
      (destructuring-bind (keyword &rest releases) changes
        (assert (eq keyword :changes))
        (dolist (release releases)
          (write-release release stream))))))
