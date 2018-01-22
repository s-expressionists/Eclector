(cl:in-package #:eclector)

(defparameter *standard-readtable*
  (make-instance 'sicl-simple-readtable:readtable))

(loop for char in '(#\Space #\Tab #\Linefeed #\Return #\Page)
      do (setf (sicl-readtable:syntax-type *standard-readtable* char)
               :whitespace))

(setf (sicl-readtable:syntax-type *standard-readtable* #\\)
      :single-escape)

(setf (sicl-readtable:syntax-type *standard-readtable* #\|)
      :multiple-escape)

(loop for (char reader-macro) in '((#\( 'left-parenthesis)
                                   (#\) 'right-parenthesis)
                                   (#\' 'single-quote)
                                   (#\" 'double-quote)
                                   (#\; 'semicolon)
                                   (#\` 'backquote)
                                   (#\, 'comma))
      do (sicl-readtable:set-macro-character
          *standard-readtable* char reader-macro))

(sicl-readtable:make-dispatch-macro-character
 *standard-readtable* #\# t)

(loop for (dispatch-char sub-char reader-macro) in '((#\# #\' 'sharpsign-single-quote)
                                                     (#\# #\( 'sharpsign-left-parenthesis)
                                                     (#\# #\. 'sharpsign-dot)
                                                     (#\# #\\ 'sharpsign-backslash)
                                                     (#\# #\b 'sharpsign-b)
                                                     (#\# #\x 'sharpsign-x)
                                                     (#\# #\o 'sharpsign-o)
                                                     (#\# #\r 'sharpsign-r)
                                                     (#\# #\* 'sharpsign-asterisk)
                                                     (#\# #\| 'sharpsign-vertical-bar)
                                                     (#\# #\a 'sharpsign-a)
                                                     (#\# #\: 'sharpsign-colon)
                                                     (#\# #\c 'sharpsign-c)
                                                     (#\# #\p 'sharpsign-p)
                                                     (#\# #\+ 'sharpsign-plus)
                                                     (#\# #\- 'sharpsign-minus)
                                                     (#\# #\= 'sharpsign-equals)
                                                     (#\# #\# 'sharpsign-sharpsign)
                                                     (#\# #\< 'sharpsign-invalid)
                                                     (#\# #\) 'sharpsign-invalid))
      do (sicl-readtable:set-dispatch-macro-character
          *standard-readtable* dispatch-char sub-char reader-macro))

(setf *readtable* (sicl-readtable:copy-readtable *standard-readtable*))
