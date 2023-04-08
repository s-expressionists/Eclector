(cl:in-package #:eclector.examples.highlight)

(defclass highlight-client (eclector.parse-result:parse-result-client)
  ((%input           :initarg  :input
                     :reader   input) ; TODO single use for whitespace error detection
   ;; Environment
   (%current-package :initarg  :current-package
                     :accessor current-package)
   (%features        :initarg  :features
                     :type     list
                     :reader   features
                     :initform (map 'list (lambda (symbol)
                                            (cons (symbol-name symbol)
                                                  (package-name (symbol-package symbol))))
                                    *features*))))

;;; Environment

(defmethod eclector.reader:call-with-current-package ((client             highlight-client)
                                                      (thunk              t)
                                                      (package-designator t))
  (let ((old-package (current-package client)))
    (setf (current-package client) (string package-designator))
    (unwind-protect
         (funcall thunk)
      (setf (current-package client) old-package))))

;;; Symbols

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator null)
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'cst:uninterned-symbol-node :name symbol-name))

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator (eql :keyword))
                                             (symbol-name       t)
                                             (internp           t))
  (make-instance 'cst:keyword-symbol-node :name symbol-name))

(defun name-of-package-p (string package)
  (or (string= string (package-name package))
      (member string (package-nicknames package) :test #'string=)))

(defmethod eclector.reader:interpret-symbol ((client            highlight-client)
                                             (input-stream      t)
                                             (package-indicator t)
                                             (symbol-name       t)
                                             (internp           t))
  (let* ((current-package-name (current-package client))
         (current-package (find-package current-package-name))
         (keyword (find-package '#:keyword))
         (cl (find-package '#:cl)))
    (flet ((standard-symbol-class (name)
             (cond ((find name lambda-list-keywords :test #'string=)
                    'cst:lambda-list-keyword-symbol-node)
                   ((not (eq :external (nth-value 1 (find-symbol symbol-name cl))))
                    (eclector.reader::%recoverable-reader-error
                     input-stream 'eclector.reader::symbol-does-not-exist
                     :position-offset (- (length symbol-name))
                     :package cl :symbol-name symbol-name
                     :report 'eclector.reader::inject-nil)
                    'cst:standard-symbol-node)
                   (t
                    'cst:standard-symbol-node))))
      (multiple-value-bind (class package internp)
          (cond ((not (eq package-indicator :current))
                 (values (cond ((name-of-package-p package-indicator keyword)
                                'cst:keyword-symbol-node)
                               ((name-of-package-p package-indicator cl)
                                (standard-symbol-class symbol-name))
                               (t
                                'cst:interned-symbol-node))
                         package-indicator internp))
                ((and current-package (eq current-package keyword))
                 'cst:keyword-symbol-node)
                ((and current-package
                      (eq :external (nth-value 1 (find-symbol symbol-name current-package))))
                 (values 'cst:interned-symbol-node current-package-name nil))
                ((eq :external (nth-value 1 (find-symbol symbol-name cl)))
                 (values (standard-symbol-class symbol-name) "CL" nil))
                (t
                 #+no (warn "No idea what's going on with ~S in ~S"
                            symbol-name current-package-name)
                 (values 'cst:interned-symbol-node current-package-name nil)))
        (apply #'make-instance class :name symbol-name
               (when package
                 (list :package package :internp internp)))))))

(defmethod eclector.reader::valid-symbol-p ((client  highlight-client)
                                            (symbol  cst:symbol-node)
                                            (purpose t))
  t)

(defmethod eclector.reader:make-structure-instance
    ((client highlight-client) (name t) (initargs t))
  (make-instance 'cst:structure-node))

(defmethod eclector.reader:check-feature-expression
    ((client highlight-client) (feature-expression cst:symbol-node))
  t)

(defmethod eclector.reader:evaluate-feature-expression
    ((client highlight-client) (feature-expression cst:symbol-node))
  (find-if (lambda (feature)
             (destructuring-bind (symbol-name . package-name) feature
               (and (string= symbol-name (cst:name feature-expression))
                    (string= package-name (cst:package feature-expression)))))
           (features client)))

(defmethod eclector.reader:evaluate-expression
    ((client highlight-client) (expression t))
  (labels ((give-up ()
             (return-from eclector.reader:evaluate-expression
               expression))
           (evaluate (expression)
             (typecase expression
               (cons
                (destructuring-bind (head &rest rest)
                    (map 'list #'evaluate expression)
                  (unless (member head '(list list* + - * /))
                    (give-up))
                  (apply head rest)))
               (cst:keyword-symbol-node
                (or (find-symbol (cst:name expression) '#:keyword)
                    (give-up)))
               (cst:interned-symbol-node
                (if (string= (cst:package expression) "CL")
                    (multiple-value-bind (symbol kind)
                        (find-symbol (cst:name expression) '#:cl)
                      (if kind
                          symbol
                          (give-up)))
                    (give-up)))
               (t
                expression))))
    (evaluate expression)))

;;; Creating S-expressions

(defmethod eclector.reader:wrap-in-quote ((client   highlight-client)
                                          (material t))
  (make-instance 'cst:quote-node :object material))

(defmethod eclector.reader:wrap-in-quasiquote ((client highlight-client)
                                               (form   t))
  (make-instance 'cst:quasiquote-node :object form))

(defmethod eclector.reader:wrap-in-unquote ((client highlight-client)
                                            (form   t))
  (make-instance 'cst:unquote-node :object form))

(defmethod eclector.reader:wrap-in-unquote-splicing ((client highlight-client)
                                                     (form   t))
  (make-instance 'cst:unquote-node :object form))

(defmethod eclector.reader:wrap-in-function ((client highlight-client)
                                             (name   t))
  (make-instance 'cst:function-node :object name))

;;; Generic conversion from results to nodes

(defmethod eclector.parse-result:make-skipped-input-result
    ((client highlight-client) (stream t) (reason t) (source t))
  (let ((class (typecase reason
                 ((eql :block-comment)       'cst:block-comment-node)
                 ((cons (eql :line-comment)) 'cst:line-comment-node)
                 (t                          'cst:skipped-node))))
    (make-instance class :children '()
                         :source   source)))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result cst:node) (children t) (source t))
  ;; This is used if RESULT already is a `cst:node'.  For example, the
  ;; `interpret-symbol' method returns partly initialized
  ;; `cst:symbol-node' instances.
  (let ((existing-children (when (slot-boundp result 'cst::%children)
                             (cst:children result))))
    (cond ((and (a:length= 1 children)
                (eq result (first children)))
           (reinitialize-instance result :source source))
          (t
           (assert (not existing-children))
           (reinitialize-instance result :children children :source source)))))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client) (result t) (children list) (source t))
  (cond #+no ((and (a:lastcar children)
              ; (typep (a:lastcar children) 'cst::object-node-mixin)
              (eq result (cst:object (a:lastcar children))))
         (make-instance 'cst:feature-expression-node
                        :children (butlast children)
                        :source   source))
        ((eq (eclector.reader:labeled-object-state client result) :circular)
         ;; This happens for labeled objects which are actually
         ;; circular and could not yet be fixed up like #1# in #1=(1
         ;; #1#).  The `invalid-node' instance will be replaced by a
         ;; REFERENCE-NODE when the surrounding object (and CST) is
         ;; fixed up.  When we recover from invalid input like #1=#1#,
         ;; the `invalid-node' instance may remain in the final CST.
         (make-instance 'cst::invalid-node :object result
                                           :source source))
        (t
         (make-instance (result-node-class client result)
                        :object   result
                        :children children
                        :source   source))))

(defmethod eclector.parse-result:make-expression-result
    ((client   highlight-client)
     (result   (eql eclector.parse-result::**definition**))
     (children t)
     (source   t))
  (let ((target (nth-value 2 (eclector.reader:labeled-object-state
                              client children))))
    (make-instance 'cst::definition-node :label    1
                                         :children (list target)
                                         :source   source)))

(defmethod eclector.parse-result:make-expression-result
    ((client highlight-client)
     (result (eql eclector.parse-result::**reference**))
     (children t)
     (source t))
  (make-instance 'cst::reference-node :label  1
                                      :source source))

(defmethod eclector.reader:fixup ((client highlight-client) (object cst:node) (seen-objects t))
  (flet ((fixup-child (cell)
           (let* ((child          (car cell))
                  (current-object (cst:object child)))
             (eclector.reader:fixup-case (client current-object)
                 (()
                   (eclector.reader:fixup client child seen-objects))
                 (()
                   (let ((reference (eclector.parse-result:make-expression-result
                                     client
                                     eclector.parse-result:**reference**
                                     current-object
                                     (cst:source child)))) ; let ((parse-result (nth-value 2 (eclector.reader:labeled-object-state client object))))
                     (setf (cst::%parent reference) object)
                     (setf (car cell) reference))
                   #+no (let ((new-class (result-node-class client object*)))
                          (change-class object new-class :object object* :children '())))))))
    (mapl #'fixup-child (cst:children object))))

(defmethod result-node-class ((client highlight-client) (result number))
  'cst:number-node)

(defmethod result-node-class ((client highlight-client) (result character))
  'cst:character-node)

(defmethod result-node-class ((client highlight-client) (result symbol))
  'cst:symbol-node)

(defmethod result-node-class ((client highlight-client) (result array))
  'cst:array-node)

(defmethod result-node-class ((client highlight-client) (result string))
  'cst:string-node)

(defmethod result-node-class ((client highlight-client) (result vector))
  'cst:vector-node)

(defmethod result-node-class ((client highlight-client) (result cons))
  'cst:cons-node)

(defmethod result-node-class ((client highlight-client) (result pathname))
  'cst:pathname-node)

;;;

(defun read-stuff (input &key package
                              (client (make-instance 'highlight-client
                                                     :input           input
                                                     :current-package package)))
  (let* ((stream    (make-string-input-stream input))
         (eof-value stream)
         (results   '())
         (errors    '()))
    (flet ((record-error (condition)
             (let* ((offset (eclector.base:position-offset condition))
                    (start  (+ (eclector.base:stream-position condition)
                               offset))
                    (end    (+ start (eclector.base:range-length condition))))
               (push (cst:make-syntax-error start end condition) errors))))
      (handler-bind ((eclector.base:stream-position-reader-error
                       (lambda (condition)
                         (record-error condition)
                         (eclector.reader:recover))))
        (loop
          (multiple-value-bind (parse-result orphan-results)
              (eclector.parse-result:read client stream nil eof-value)
            (let* ((result-list (if (eq parse-result eof-value) '() (list parse-result)))
                   (new-results (merge 'list result-list orphan-results #'<
                                       :key #'cst:start)))
              (setf results (nconc results new-results)))
            (when (eq parse-result eof-value)
              (return))))))
    (values (cst:make-cst results) errors)))
