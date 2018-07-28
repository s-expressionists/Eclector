(cl:in-package #:eclector.reader)

(defun set-standard-syntax-types (readtable)
  (flet (((setf syntax) (syntax-type char)
           (setf (eclector.readtable:syntax-type readtable char)
                 syntax-type)))
    (setf (syntax #\Space)    :whitespace
          (syntax #\Tab)      :whitespace
          (syntax #\Linefeed) :whitespace
          (syntax #\Return)   :whitespace
          (syntax #\Page)     :whitespace
          (syntax #\\)        :single-escape
          (syntax #\|)        :multiple-escape)))

(defun set-standard-macro-characters (readtable)
  (loop for (char reader-macro) in '((#\( left-parenthesis)
                                     (#\) right-parenthesis)
                                     (#\' single-quote)
                                     (#\" double-quote)
                                     (#\; semicolon)
                                     (#\` backquote)
                                     (#\, comma))
        do (eclector.readtable:set-macro-character
            readtable char reader-macro)))

(defun set-standard-dispatch-macro-characters (readtable)
  (eclector.readtable:make-dispatch-macro-character
   readtable #\# t)

  (loop for (dispatch-char sub-char reader-macro) in '((#\# #\' sharpsign-single-quote)
                                                       (#\# #\( sharpsign-left-parenthesis)
                                                       (#\# #\. sharpsign-dot)
                                                       (#\# #\\ sharpsign-backslash)
                                                       (#\# #\b sharpsign-b)
                                                       (#\# #\x sharpsign-x)
                                                       (#\# #\o sharpsign-o)
                                                       (#\# #\r sharpsign-r)
                                                       (#\# #\* sharpsign-asterisk)
                                                       (#\# #\| sharpsign-vertical-bar)
                                                       (#\# #\a sharpsign-a)
                                                       (#\# #\: sharpsign-colon)
                                                       (#\# #\c sharpsign-c)
                                                       (#\# #\p sharpsign-p)
                                                       (#\# #\+ sharpsign-plus)
                                                       (#\# #\- sharpsign-minus)
                                                       (#\# #\= sharpsign-equals)
                                                       (#\# #\# sharpsign-sharpsign)
                                                       (#\# #\< sharpsign-invalid)
                                                       (#\# #\) sharpsign-invalid))
        do (eclector.readtable:set-dispatch-macro-character
            readtable dispatch-char sub-char reader-macro)))

(defun set-standard-syntax-and-macros (readtable)
  (set-standard-syntax-types readtable)
  (set-standard-macro-characters readtable)
  (set-standard-dispatch-macro-characters readtable))

(defparameter *standard-readtable*
  (let ((readtable (make-instance 'eclector.readtable.simple:readtable)))
    (set-standard-syntax-and-macros readtable)
    readtable))

(setf *readtable* (eclector.readtable:copy-readtable *standard-readtable*))
