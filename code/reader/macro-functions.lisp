(cl:in-package #:eclector.reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro WITH-PRESERVED-BACKQUOTE-CONTEXT.
;;;
;;; This macros allows backquotes in sub-forms if and only if
;;; backquotes are allowed in the main form.

(defmacro with-preserved-backquote-context (&body body)
  `(let ((*backquote-in-subforms-allowed-p* *backquote-allowed-p*))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for semicolon.
;;;
;;; We read characters until end-of-file or until we have read a
;;; newline character.  Since reading a comment does not generate an
;;; object, the semicolon reader must indicate that fact by returning
;;; zero values.

(defun semicolon (stream char)
  (declare (ignore char))
  (loop with state = :semicolon
        for char = (read-char stream nil nil t)
        until (or (null char) (eql char #\Newline))
        if (and (eq state :semicolon) (char= char #\;))
        count 1 into semicolons
        else
        do (setf state nil)
        finally (when (eql char #\Newline)
                  (setf *skip-reason* (cons :line-comment (1+ semicolons)))
                  (unread-char char stream)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for single quote.
;;;
;;; They HyperSpec says that the reader signals an error if
;;; end-of-file is encountered before an object has been entirely
;;; parsed, independently of whether EOF-ERROR-P is true or not.  For
;;; that reason, we call the reader recursively with the value of
;;; EOF-ERROR-P being T.

(defun single-quote (stream char)
  (declare (ignore char))
  (let ((material (with-preserved-backquote-context
                      (read stream t nil t))))
    (wrap-in-quote *client* material)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for double quote.
;;;
;;; We identify a single escape character by its syntax type, so that
;;; if a user wants a different escape chacacter, we can handle that.
;;;
;;; Furthermore, They HyperSpec says that the reader signals an error
;;; if end-of-file is encountered before an object has been entirely
;;; parsed, independently of whether EOF-ERROR-P is true or not.  For
;;; that reason, we call READ-CHAR with the value of EOF-ERROR-P being
;;; T.
;;;
;;; We accumulate characters in an adjustable vector.  However, the
;;; HyperSpec says that we must return a SIMPLE-STRING.  For that
;;; reason, we call COPY-SEQ in the end.  COPY-SEQ is guaranteed to
;;; return a simple vector.

(defun double-quote (stream char)
  (loop with result = (make-array 100
                                  :element-type 'character
                                  :adjustable t
                                  :fill-pointer 0)
        with readtable = *readtable*
        for char2 = (read-char stream t nil t)
        until (eql char2 char)
        when (eq (eclector.readtable:syntax-type readtable char2) :single-escape)
        do (setf char2 (read-char stream t nil t))
        do (vector-push-extend char2 result)
        finally (return (copy-seq result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for backquote and comma.
;;;
;;; The control structure we use for backquote requires some
;;; explanation.
;;;
;;; The HyperSpec says that backquote and comma are allowed only
;;; inside lists and vectors.  Since READ can be called recursively
;;; from other functions as well (such as the reader for arrays, or
;;; user-defined readers), we somehow need to detect whether we are
;;; about to read a list or a vector.
;;;
;;; Perhaps the easiest way to do this would be to bind a flag to
;;; false in all readers EXCEPT the ones for lists and vectors.  This
;;; solution would require a programmer who writes a custom reader
;;; macro to do the same, or else backquote and comma would be
;;; processed in sub-forms.  Clearly, this solution is not so great.
;;;
;;; So we need a way for readers for lists and vectors to explicitly
;;; allow for backquote and comma, whereas BY DEFAULT, they should not
;;; be allowed.  We solve this by introducting two variables:
;;; *BACKQUOTE-ALLOWED-P* and *BACKQUOTE-IN-SUBFORMS-ALLOWED-P*.
;;; Initially the two are TRUE.  Whenever READ is called, it binds the
;;; variable *BACKQUOTE-ALLOWED-P* to the value of
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P*, and it binds
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P* to FALSE.  If no special action
;;; is taken, wheen READ is called recursively from a reader macro,
;;; the value of *BACKQUOTE-ALLOWED-P* will be FALSE.  When one of the
;;; reader macros left-parenthesis, sharpsign-left-parenthesis,
;;; backquote, or comma is called, before the recursive call to READ,
;;; *BACKQUOTE-IN-SUBFORMS-ALLOWED-P* is bound to the value of
;;; *BACKQUOTE-ALLOWED-P*.  Consequenctly, if the list or the vector
;;; is read in a context where backquote is not allowed, then it will
;;; not be allowed in subforms either, for instance if the list or the
;;; vector is inside an array.  But if the list or the vector is read
;;; in a context where backquote is allowed, then it will be allowed
;;; in subforms as well.
;;;
;;; The HyperSpec explicitly encourages us (see section 2.4.6.1) to
;;; follow the example of Scheme for representing backquote
;;; expression.  We see no reason for choosing a different
;;; representation, so we use (QUASIQUOTE <form>), (UNQUOTE <form>),
;;; and (UNQUOTE-SPLICING <form>).  Then we define QUASIQUOTE as a
;;; macro that expands to a CL form that will build the final data
;;; structure.

(defun backquote (stream char)
  (declare (ignore char))
  (unless *backquote-allowed-p*
    (%reader-error stream 'invalid-context-for-backquote))
  (let ((*backquote-depth* (1+ *backquote-depth*)))
    (with-preserved-backquote-context
      (wrap-in-quasiquote *client* (read stream t nil t)))))

(defun comma (stream char)
  (declare (ignore char))
  (let* ((inside-backquote-p (plusp *backquote-depth*))
         (char2 (read-char stream t nil t))
         (at-sign-p (case char2
                      ((#\@ #\.) t)
                      (t (unread-char char2 stream))))
         (*backquote-depth* (1- *backquote-depth*)))
    (unless inside-backquote-p
      (%reader-error stream 'comma-not-inside-backquote :at-sign-p at-sign-p))
    (with-preserved-backquote-context
      (let ((form (handler-case
                      (read stream t nil t)
                    (end-of-list ()
                      (%reader-error stream 'object-must-follow-comma
                                     :at-sign-p at-sign-p)))))
        (if at-sign-p
            (wrap-in-unquote-splicing *client* form)
            (wrap-in-unquote *client* form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for left-parenthesis and right-parenthesis.
;;;
;;; The HyperSpec says that right-parenthesis is a macro character.
;;; In the reader macro for left-parenthesis, we can not just read
;;; until we find a right parenthesis, because it is possible that
;;; some other character has been assigned the same meaning, and we
;;; need to handle that situation too.
;;;
;;; Another problem we need to solve is that of the CONSING-DOT.  The
;;; HyperSpec says that it is a token.  For that reason, we can not
;;; just read characters and look for a single period, because it is
;;; possible that the single dot has a different syntax type in this
;;; particular readtable.  Furthermore, we must handle error
;;; situations such as an attempt to use more than one dot in a list,
;;; or having zero or strictly more than one expression following a
;;; dot.
;;;
;;; We solve these problems as follows: the reader macro for a right
;;; parenthesis calls SIGNAL with a particular condition (of type
;;; END-OF-LIST).  In situations where the right parenthesis is
;;; allowed, there will be a handler for this condition type.
;;; Therefore, in that situation, the call to SIGNAL will not return.
;;; If the call to SIGNAL returns, we signal and ERROR, because then
;;; the right parenthesis was read in a context where it is not
;;; allowed.
;;;
;;; The reader macro for left parenthesis manages two local variables,
;;; REVERSED-RESULT and TAIL.  The variable REVERSED-RESULT is used to
;;; accumulate elements of the list (preceding a possible consing dot)
;;; being read, in reverse order.  A handler for END-OF-LIST is
;;; established around the recursive calls to READ inside the reader
;;; macro function.  When this handler is invoked, it calls NRECONC to
;;; reverse the value of REVERSED-RESULT and attach the value of TAIL
;;; to the end.  Normally, the value of TAIL is NIL, so the handler
;;; will create and return a proper list containing the accumulated
;;; elements.
;;;
;;; We use a special variable name *CONSING-DOT-ALLOWED-P* to
;;; determine the contexts in which a consing dot is allowed.
;;; Whenever the token parser detects a consing dot, it examines this
;;; variable, and if it is true it returns the unique CONSING-DOT
;;; token, and if it is false, signals an error.  Initially, this
;;; variable has the value FALSE.  Whenever the reader macro for left
;;; parenthesis is called, it binds this variable to TRUE.  When a
;;; recursive call to READ returns with the consing dot as a value,
;;; the reader macro for left parenthesis does three things.  First it
;;; SETS (as opposed to BINDS) *CONSING-DOT-ALLOWED-P* to FALSE, so
;;; that if a second consing dot should occur, then the token reader
;;; signals an error.  Second, it establishes a nested handler for
;;; END-OF-LIST, so that if a right parenthesis should occur
;;; immediately after the consing dot, then an error is signaled.
;;; With this handler established, READ is called.  If it returns
;;; normally, then the return value becomes the value of the variable
;;; TAIL.  Third, it calls READ again without any nested handler
;;; established.  This call had better result in a right parenthesis,
;;; so that END-OF-LIST is signaled, which is caught by the outermost
;;; handler and the correct list is built and returned.  If this call
;;; should return normally, we have a problem, because this means that
;;; there was a second subform after the consing dot in the list, so
;;; we signal an ERROR.

(defun left-parenthesis (stream char)
  (declare (ignore char))
  (let ((reversed-result '())
        (tail nil)
        (*consing-dot-allowed-p* t))
    (with-preserved-backquote-context
      (handler-case
          (loop for object = (let ((*consing-dot-allowed-p* nil))
                               (read stream t nil t))
                then (read stream t nil t)
                if (eq object *consing-dot*)
                do (setf *consing-dot-allowed-p* nil)
                   (handler-case
                       (setf tail (read stream t nil t))
                     (end-of-list ()
                       (%reader-error stream 'object-must-follow-consing-dot)))
                   ;; This call to read must not succeed.
                   (read stream t nil t)
                   (%reader-error stream 'multiple-objects-following-consing-dot)
                else
                do (push object reversed-result))
        (end-of-list ()
          (return-from left-parenthesis
            (nreconc reversed-result tail)))))))

(defun right-parenthesis (stream char)
  (declare (ignore char))
  ;; If the call to SIGNAL returns, then there is no handler for this
  ;; condition, which means that the right parenthesis was found in a
  ;; context where it is not allowed.
  (signal *end-of-list*)
  (%reader-error stream 'invalid-context-for-right-parenthesis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign single quote.

(defun sharpsign-single-quote (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-single-quote parameter))
  (with-preserved-backquote-context
    (let ((name (read stream t nil t)))
      (if *read-suppress*
          nil
          `(function ,name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign left parenthesis.

(defun sharpsign-left-parenthesis (stream char parameter)
  (declare (ignore char))
  (with-preserved-backquote-context
    (flet ((next-element ()
             (handler-case
                 (values (read stream t nil t) t)
               (end-of-list ()
                 (values nil nil)))))
      (cond
        (*read-suppress*
         (loop for elementp = (nth-value 1 (next-element))
               while elementp))
        ((null parameter)
         (loop with result = (make-array 10 :adjustable t :fill-pointer 0)
               for (element elementp) = (multiple-value-list (next-element))
               while elementp
               do (vector-push-extend element result)
               finally (return (coerce result 'simple-vector))))
        (t
         (loop with result = (make-array parameter)
               for index from 0
               for (element elementp) = (multiple-value-list
                                         (next-element))
               while elementp
               when (< index parameter)
               do (setf (aref result index) element)
               finally (cond
                         ((and (zerop index) (plusp parameter))
                          (%reader-error stream 'no-elements-found
                                         :array-type 'vector
                                         :expected-number parameter))
                         ((> index parameter)
                          (%reader-error stream 'too-many-elements
                                         :array-type 'vector
                                         :expected-number parameter
                                         :number-found index)))
                       (return
                         (if (< index parameter)
                             (fill result (aref result (1- index))
                                   :start index)
                             result))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign dot.

(defun sharpsign-dot (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-dot parameter))
  (with-preserved-backquote-context
    (cond
      ((not *read-eval*)
       (%reader-error stream 'read-time-evaluation-inhibited))
      (*read-suppress*
       (read stream t nil t))
      (t
       (let ((expression (read stream t nil t)))
         (handler-case
             (evaluate-expression *client* expression)
           (error (condition)
             (%reader-error stream 'read-time-evaluation-error
                            :expression expression
                            :original-condition condition))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign backslash.

(defparameter *character-names*
  (alexandria:alist-hash-table '(("NEWLINE"   . #.(code-char 10))
                                 ("SPACE"     . #.(code-char 32))
                                 ("RUBOUT"    . #.(code-char 127))
                                 ("PAGE"      . #.(code-char 12))
                                 ("TAB"       . #.(code-char 9))
                                 ("BACKSPACE" . #.(code-char 8))
                                 ("RETURN"    . #.(code-char 13))
                                 ("LINEFEED"  . #.(code-char 10)))
                               :test 'equal))

(defun find-standard-character (name)
  (gethash name *character-names*))

(defun sharpsign-backslash (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-backslash parameter))
  (let ((char1 (read-char stream t nil t))
        (token nil))
    (flet ((next-char (&optional eof-error-p)
             (let* ((char (read-char stream eof-error-p nil t))
                    (syntax-type (when char
                                   (eclector.readtable:syntax-type
                                    *readtable* char))))
               (values char syntax-type)))
           (collect-char (char)
             (cond
               (token (vector-push-extend char token))
               (t (setf token (make-array 10
                                          :element-type 'character
                                          :adjustable t
                                          :fill-pointer 2)
                        (aref token 0) char1
                        (aref token 1) char)))))
      (tagbody
       even-escapes
         (multiple-value-bind (char syntax-type) (next-char)
           (ecase syntax-type
             ((nil)
              (go terminate))
             ((:constituent :non-terminating-macro)
              (collect-char char)
              (go even-escapes))
             (:single-escape
              (collect-char (read-char stream t nil t))
              (go even-escapes))
             (:multiple-escape
              (go odd-escapes))
             (:terminating-macro
              (unread-char char stream)
              (go terminate))
             (:whitespace
              (when *preserve-whitespace*
                (unread-char char stream))
              (go terminate))))
       odd-escapes
         (multiple-value-bind (char syntax-type) (next-char t)
           (ecase syntax-type
             ((:constituent :terminating-macro
               :non-terminating-macro :whitespace)
              (collect-char char)
              (go odd-escapes))
             (:single-escape
              (collect-char (read-char stream t nil t))
              (go odd-escapes))
             (:multiple-escape
              (go even-escapes))))
       terminate
         (return-from sharpsign-backslash
           (cond
             (*read-suppress* nil)
             (token (alexandria:if-let ((char (find-character
                                               *client* (string-upcase token))))
                      char
                      (%reader-error stream 'unknown-character-name
                                     :name token)))
             (t char1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign B, X, O and R.

(defun read-rational (stream base)
  (let ((readtable *readtable*)
        (read-suppress *read-suppress*))
    (labels ((next-char (eof-error-p)
               (alexandria:if-let ((char (read-char stream eof-error-p nil t)))
                 (values char (eclector.readtable:syntax-type readtable char))
                 (values nil nil)))
             (digit-expected (char)
               (%reader-error stream 'digit-expected
                              :character-found char :base base))
             (ensure-digit (char)
               (let ((value (digit-char-p char base)))
                 (cond (value)
                       (read-suppress
                        1)
                       (t
                        (digit-expected char)))))
             (maybe-sign ()
               (multiple-value-bind (char type) (next-char t)
                 (ecase type
                   ((:whitespace :terminating-macro
                     :non-terminating-macro :single-escape :multiple-escape)
                    (digit-expected char))
                   (:constituent
                    (if (char= char #\-)
                        (values -1 0)
                        (values 1 (ensure-digit char)))))))
             (integer (empty-allowed &optional /-allowed initial-value)
               (let ((value initial-value))
                 (tagbody
                    (when empty-allowed (go rest))
                    (multiple-value-bind (char type) (next-char t)
                      (ecase type
                        ((:whitespace
                          :terminating-macro :non-terminating-macro
                          :single-escape :multiple-escape)
                         (digit-expected char))
                        (:constituent
                         (setf value (ensure-digit char)))))
                  rest
                    (multiple-value-bind (char type) (next-char nil)
                      (ecase type
                        ((nil)
                         (return-from integer value))
                        (:whitespace
                         (when *preserve-whitespace*
                           (unread-char char stream))
                         (return-from integer value))
                        (:terminating-macro
                         (unread-char char stream)
                         (return-from integer value))
                        ((:non-terminating-macro
                          :single-escape :multiple-escape)
                         (digit-expected char))
                        (:constituent
                         (if (and /-allowed (eql char #\/))
                             (return-from integer (values value t))
                             (setf value (+ (* base (or value 0))
                                            (ensure-digit char))))
                         (go rest))))))))
      (multiple-value-bind (sign numerator) (maybe-sign)
        (multiple-value-bind (numerator slashp) (integer (= sign 1) t numerator)
          (let ((denominator (when slashp (integer nil))))
            (unless read-suppress
              (* sign (if denominator
                          (/ numerator denominator)
                          numerator)))))))))

(defun sharpsign-b (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-b parameter))
  (read-rational stream 2.))

(defun sharpsign-x (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-x parameter))
  (read-rational stream 16.))

(defun sharpsign-o (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-o parameter))
  (read-rational stream 8.))

(defun sharpsign-r (stream char parameter)
  (declare (ignore char))
  (let ((radix (cond ((not parameter)
                      (numeric-parameter-not-supplied stream 'sharpsign-r)
                      36)
                     ((not (<= 2 parameter 36))
                      (if *read-suppress*
                          36
                          (%reader-error
                           stream 'invalid-radix :radix parameter)))
                     (t
                      parameter))))
    (read-rational stream radix)))

(defun sharpsign-asterisk (stream char parameter)
  (declare (ignore char))
  (let ((read-suppress *read-suppress*)
        (readtable *readtable*))
    (flet ((next-bit ()
             (let ((char (read-char stream nil nil t)))
               (multiple-value-bind (syntax-type value)
                   (unless (null char)
                     (values (eclector.readtable:syntax-type
                              readtable char)
                             (digit-char-p char 2)))
                 (when (eq syntax-type :terminating-macro)
                   (unread-char char stream))
                 (cond
                   ((member syntax-type '(nil :whitespace :terminating-macro))
                    nil)
                   (read-suppress
                    t)
                   ((null value)
                    (%reader-error stream 'digit-expected
                                   :character-found char
                                   :base 2.))
                   (t
                    value))))))
      (cond
        (read-suppress
         (loop for value = (next-bit) while value))
        ((null parameter)
         (loop with bits = (make-array 10 :element-type 'bit
                                          :adjustable t :fill-pointer 0)
               for value = (next-bit)
               while value
               do (vector-push-extend value bits)
               finally (return (coerce bits 'simple-bit-vector))))
        (t
         (loop with result = (make-array parameter :element-type 'bit)
               for index from 0
               for value = (next-bit)
               while value
               when (< index parameter)
               do (setf (sbit result index) value)
               finally (cond
                         ((and (zerop index) (plusp parameter))
                          (%reader-error stream 'no-elements-found
                                         :array-type 'bit-vector
                                         :expected-number parameter))
                         ((> index parameter)
                          (%reader-error stream 'too-many-elements
                                         :array-type 'bit-vector
                                         :expected-number parameter
                                         :number-found index)))
                       (return
                         (if (< index parameter)
                             (fill result (sbit result (1- index))
                                   :start index)
                             result))))))))

(defun sharpsign-vertical-bar (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-vertical-bar parameter))
  (loop for char = (read-char stream t nil t)
        do (cond ((eql char #\#)
                  (let ((char2 (read-char stream t nil t)))
                    (if (eql char2 #\|)
                        (sharpsign-vertical-bar stream #\| nil)
                        (unread-char char2 stream))))
                 ((eql char #\|)
                  (let ((char2 (read-char stream t nil t)))
                    (if (eql char2 #\#)
                        (progn
                          (setf *skip-reason* :block-comment)
                          (return-from sharpsign-vertical-bar (values)))
                        (unread-char char2 stream))))
                 (t
                  nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign A.

(defun determine-dimensions (stream rank initial-contents)
  (labels ((rec (rank initial-contents)
             (cond ((zerop rank)
                    '())
                   ((not (typep initial-contents 'alexandria:proper-sequence))
                    (%reader-error stream 'read-object-type-error
                                   :expected-type 'sequence
                                   :datum initial-contents))
                   (t
                    (let ((length (length initial-contents)))
                      (if (zerop length)
                          (make-list rank :initial-element 0)
                          (list* length
                                 (rec (1- rank) (elt initial-contents 0)))))))))
    (rec rank initial-contents)))

(defun check-dimensions (stream dimensions initial-contents)
  (labels ((rec (first rest axis initial-contents)
             (cond
               ((not first))
               ((not (eql (length initial-contents) (or first 0)))
                (%reader-error stream 'incorrect-initialization-length
                               :array-type 'array
                               :axis axis
                               :expected-length first
                               :datum initial-contents))
               (t
                (every (lambda (subseq)
                         (rec (first rest) (rest rest) (1+ axis) subseq))
                       initial-contents)))))
    (rec (first dimensions) (rest dimensions) 0 initial-contents)))

(defun sharpsign-a (stream char parameter)
  (declare (ignore char))
  (unless parameter
    (numeric-parameter-not-supplied stream 'sharpsign-a))
  (if *read-suppress*
      (read stream t nil t)
      (let* ((init (read stream t nil t))
             (dimensions (determine-dimensions stream parameter init)))
        (check-dimensions stream dimensions init)
        (make-array dimensions :initial-contents init))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign colon.

(defun symbol-from-token (stream token token-escapes package-marker)
  (when *read-suppress*
    (return-from symbol-from-token nil))
  (when package-marker
    (%reader-error stream 'uninterned-symbol-must-not-contain-package-marker
                   :stream-position (if (eq package-marker t)
                                        nil
                                        package-marker)
                   :token token))
  (convert-according-to-readtable-case token token-escapes)
  (interpret-symbol *client* stream nil (copy-seq token) nil))

(defun sharpsign-colon (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-colon parameter))
  (let ((readtable *readtable*)
        (token (make-array 10
                           :element-type 'character
                           :adjustable t
                           :fill-pointer 0))
        (escape-ranges '())
        (package-marker nil))
    (labels ((push-char (char escapesp)
               (when (and (not escapesp)
                          (char= char #\:)
                          (not package-marker))
                 (setf package-marker (or (ignore-errors (file-position stream))
                                          t)))
               (vector-push-extend char token))
             (start-escape ()
               (push (cons (length token) nil) escape-ranges))
             (end-escape ()
               (setf (cdr (first escape-ranges)) (length token)))
             (read-char-handling-eof (eof-error-p)
               (let ((char (read-char stream eof-error-p nil t)))
                 (when (null char)
                   (return-symbol))
                 (values char (eclector.readtable:syntax-type readtable char))))
             (return-symbol ()
               (when (not (null escape-ranges))
                 (setf escape-ranges (nreverse escape-ranges)))
               (return-from sharpsign-colon
                 (symbol-from-token stream token escape-ranges package-marker))))
      (tagbody
       even-escapes
         (multiple-value-bind (char syntax-type) (read-char-handling-eof nil)
           (ecase syntax-type
             (:whitespace
              (when *preserve-whitespace*
                (unread-char char stream))
              (return-symbol))
             (:terminating-macro
              (unread-char char stream)
              (return-symbol))
             (:single-escape
              (start-escape)
              (push-char (read-char-handling-eof t) t)
              (end-escape)
              (go even-escapes))
             (:multiple-escape
              (start-escape)
              (go odd-escapes))
             ((:constituent :non-terminating-macro)
              (push-char char nil)
              (go even-escapes))))
       odd-escapes
         (multiple-value-bind (char syntax-type) (read-char-handling-eof t)
           (case syntax-type
             (:single-escape
              (push-char (read-char-handling-eof t) t)
              (go odd-escapes))
             (:multiple-escape
              (end-escape)
              (go even-escapes))
             (t
              (push-char char t)
              (go odd-escapes))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign C.

(defun sharpsign-c (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-c parameter))
  (let ((parts (read stream t nil t)))
    (cond
      (*read-suppress*
       nil)
      ((typep parts '(cons real (cons real null)))
       (complex (first parts) (second parts)))
      (t
       (%reader-error stream 'read-object-type-error
                      :datum parts
                      :expected-type '(cons real (cons real null)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign S.

(defun sharpsign-s (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-s parameter))
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharpsign-s nil))
  (let ((char (read-char stream t nil t)))
    (unless (char= char #\()
      (%reader-error stream 'non-list-following-sharpsign-s)))
  (with-preserved-backquote-context
    (labels ((read* ()
               (handler-case
                   (read stream t nil t)
                 (end-of-list (condition)
                   condition)))
             (read-type ()
               (let ((type (read*)))
                 (cond ((symbolp type)
                        type)
                       ((eq type *end-of-list*)
                        (%reader-error stream 'no-structure-type-name-found))
                       (t
                        (%reader-error stream 'structure-type-name-is-not-a-symbol
                                       :datum type)))))
             (read-slot-name ()
               (let ((name (read*)))
                 (cond ((symbolp name)
                        name)
                       ((eq name *end-of-list*)
                        nil)
                       (t
                        (%reader-error stream 'slot-name-is-not-a-symbol
                                       :datum name)))))
             (read-slot-value (slot-name)
               (let ((value (read*)))
                 (if (eq value *end-of-list*)
                     (%reader-error stream 'no-slot-value-found
                                    :slot-name slot-name)
                     value))))
      (make-structure-instance
       *client* (read-type) (loop for slot-name = (read-slot-name)
                                  for slot-value = (when slot-name
                                                     (read-slot-value slot-name))
                                  while slot-name
                                  collect slot-name
                                  collect slot-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macro for sharpsign P.

(defun sharpsign-p (stream char parameter)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-p parameter))
  (let ((expression (read stream t nil t)))
    (unless *read-suppress*
      (unless (stringp expression)
        (%reader-error stream 'read-object-type-error
                       :expected-type 'string
                       :datum expression))
      (parse-namestring expression))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign + and sharpsign -.

(deftype feature-expression-operator ()
  '(member :not :or :and))

(defun check-standard-feature-expression (feature-expression)
  (flet ((lose (stream-condition no-stream-condition &rest arguments)
           (alexandria:if-let ((stream *input-stream*))
             (apply #'%reader-error stream stream-condition arguments)
             (apply #'error no-stream-condition arguments))))
    (unless (or (symbolp feature-expression)
                (alexandria:proper-list-p feature-expression))
      (lose 'feature-expression-type-error/reader
            'feature-expression-type-error
            :datum feature-expression
            :expected-type '(or symbol cons)))
    (when (consp feature-expression)
      (destructuring-bind (operator &rest operands) feature-expression
        (unless (typep operator 'feature-expression-operator)
          (lose 'feature-expression-type-error/reader
                'feature-expression-type-error
                :datum operator
                :expected-type 'feature-expression-operator))
        (when (and (eq operator :not)
                   (not (alexandria:length= 1 operands)))
          (lose 'single-feature-expected/reader 'single-feature-expected
                :features (cdr feature-expression)))))))

(defun evaluate-standard-feature-expression
    (feature-expression
     &key
     (check 'check-standard-feature-expression)
     (recurse 'evaluate-standard-feature-expression))
  (funcall check feature-expression)
  (typecase feature-expression
    (symbol
     (member feature-expression *features* :test #'eq))
    ((cons (eql :not))
     (not (funcall recurse (second feature-expression))))
    ((cons (eql :or))
     (some recurse (rest feature-expression)))
    ((cons (eql :and))
     (every recurse (rest feature-expression)))))

(defun sharpsign-plus-minus (stream char parameter invertp)
  (declare (ignore char))
  (unless (null parameter)
    (numeric-parameter-ignored stream 'sharpsign-p parameter))
  (let ((feature-expression
          (let ((*package* (find-package '#:keyword))
                (*read-suppress* nil))
            (read stream t nil t))))
    (with-preserved-backquote-context
      (if (alexandria:xor (evaluate-feature-expression
                           *client* feature-expression)
                          invertp)
          (read stream t nil t)
          (let ((reason (if invertp
                            :sharpsign-minus
                            :sharpsign-plus)))
            (setf *skip-reason* (cons reason feature-expression))
            (let ((*read-suppress* t))
              (read stream t nil t))
            (values))))))

(defun sharpsign-plus (stream char parameter)
  (sharpsign-plus-minus stream char parameter nil))

(defun sharpsign-minus (stream char parameter)
  (sharpsign-plus-minus stream char parameter t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign < and sharpsign )

(defun sharpsign-invalid (stream char parameter)
  (declare (ignore parameter))
  (%reader-error stream 'sharpsign-invalid :character-found char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign equals.
;;;
;;; When the SHARPSIGN-EQUALS reader macro encounters #N=EXPRESSION,
;;; it associates a marker object with N in the hash-table bound to
;;; *LABELS*. The marker object is of the form
;;;
;;;   ((FINALP) . FINAL-OBJECT)
;;;
;;; where FINALP and FINAL-OBJECT are initially NIL. The cons cell
;;; (FINALP) is called the temporary object of the marker object.
;;;
;;; If #N# is encountered, the marker for N is looked up in *LABELS*
;;; and FINALP is examined. If FINALP is true, FINAL-OBJECT can be
;;; returned as the result of reading #N#. However, if FINALP is false
;;; (this can happen while READing EXPRESSION if #N=EXPRESSION is
;;; circular), the temporary object is returned as the result of
;;; reading #N# and a deferred fixup step will be necessary. This
;;; fixup happens in READ-AUX.
;;;
;;; After reading EXPRESSION, the resulting object is stored in the
;;; cdr as FINAL-OBJECT and FINALP within the temporary object is set
;;; to true. Subsequent #N# encounters can directly return
;;; FINAL-OBJECT as described above.

(declaim (inline make-fixup-marker
                 fixup-marker-temporary
                 fixup-marker-final-p (setf fixup-marker-final-p)
                 fixup-marker-final (setf fixup-marker-final)))

(defun make-fixup-marker ()
  (let ((temporary (list nil)))
    (cons temporary nil)))

(defun fixup-marker-temporary (marker)
  (car marker))

(defun fixup-marker-final-p (marker)
  (car (fixup-marker-temporary marker)))

(defun (setf fixup-marker-final-p) (new-value marker)
  (setf (car (fixup-marker-temporary marker)) new-value))

(defun fixup-marker-final (marker)
  (cdr marker))

(defun (setf fixup-marker-final) (new-value marker)
  (setf (cdr marker) new-value))

(defun sharpsign-equals (stream char parameter)
  (declare (ignore char))
  (when (null parameter)
    (numeric-parameter-not-supplied stream 'sharpsign-equals))
  (when (nth-value 1 (gethash parameter *labels*))
    (%reader-error stream 'sharpsign-equals-label-defined-more-than-once
                   :label parameter))
  (with-preserved-backquote-context
    (let ((marker (make-fixup-marker)))
      (setf (gethash parameter *labels*) marker)
      ;; FIXME Do we need to transmit EOF-ERROR-P through reader macros?
      (let ((result (read stream t nil t)))
        (when (eq result (fixup-marker-temporary marker))
          (%reader-error stream 'sharpsign-equals-only-refers-to-self
                         :label parameter))
        (setf (fixup-marker-final marker) result
              (fixup-marker-final-p marker) t)
        result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader macros for sharpsign sharpsign.

(defun sharpsign-sharpsign (stream char parameter)
  (declare (ignore char))
  (when (null parameter)
    (numeric-parameter-not-supplied stream 'sharpsign-equals))
  (multiple-value-bind (marker definedp) (gethash parameter *labels*)
    (cond ((not definedp)
           (%reader-error stream 'sharpsign-sharpsign-undefined-label
                          :label parameter))
          ;; If the final object has already been supplied, use it.
          ((fixup-marker-final-p marker)
           (fixup-marker-final marker))
          ;; Else, we must use the temporary object and it will be
          ;; fixed up later.
          (t
           (fixup-marker-temporary marker)))))
