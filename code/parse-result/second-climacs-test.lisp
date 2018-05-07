(cl:in-package #:eclector.parse-result)

(defclass result ()
  ((start :initarg :start
          :reader  start)
   (end   :initarg :end
          :reader  end)))

(defclass expression-result (result)
  ((expression :initarg :expression
               :reader  expression)
   (children   :initarg :children
               :reader  children)))

(defclass skipped-input-result (result)
  ((reason :initarg :reason
           :reader  reason)))

(defclass second-climacs (parse-result-mixin)
  ((cache :reader   cache
          :initform (make-hash-table :test #'eql))))

(defmethod getcache ((position t) (client second-climacs))
  (gethash position (cache client)))

(defmethod (setf getcache) ((new-value t) (position t) (client second-climacs))
  (setf (gethash position (cache client)) new-value))

(defmethod make-expression-result ((client second-climacs) (result t) (children t) (source cons))
  (make-instance 'expression-result
                 :expression result
                 :children children
                 :start (car source)
                 :end (cdr source)))

(defmethod make-skipped-input-result ((client second-climacs) stream reason source)
  (make-instance 'skipped-input-result :reason reason :start (car source) :end (cdr source)))

(defmethod eclector.reader:read-common :around ((client second-climacs) input-stream eof-error-p eof-value)
  (let ((position (source-position client input-stream)))
    (if-let ((cached (getcache position client)))
      (progn
        (assert (eql (start cached) position))
        (format t "@ ~D: Using cached result ~A and skipping to ~D~%"
                position cached (end cached))
        (loop :repeat (- (end cached) position) :do (read-char input-stream))
        (values (expression cached) cached))
      (progn
        (format t "@ ~D: Parsing~%" position)
        (multiple-value-bind (expression parse-result) (call-next-method)
          (setf (getcache position client) parse-result)
          (values expression parse-result))))))

(let ((eclector.reader:*client* (make-instance 'second-climacs)))
  (read (make-string-input-stream "(1 2 #| hi |# 3)"))
  (read (make-string-input-stream "(1 2 #| hi |# 3)")))
