(cl:in-package #:eclector.reader)

;;; Labeled objects

(deftype labeled-object-state ()
  '(member :defined :circular :final :final/circular))

(defstruct (%labeled-object (:constructor %make-labeled-object ())
                            (:predicate nil)
                            (:copier nil))
  (state :defined :type labeled-object-state)
  (object nil))

(defmethod call-with-label-tracking (client thunk)
  (declare (ignore client))
  ;; Establish a binding but don't allocate the hash table yet.
  (let ((*labels* nil))
    (funcall thunk)))

(defmethod note-labeled-object (client input-stream (label integer))
  ;; Allocate hash table lazily.
  (let ((labels (or *labels*
                    (setf *labels* (make-hash-table)))))
    (setf (gethash label labels)
          (make-labeled-object client input-stream label))))

(defmethod forget-labeled-object (client (label integer))
  (declare (ignore client))
  (remhash label *labels*))

(defmethod find-labeled-object (client (label integer))
  (declare (ignore client))
  (alexandria:when-let ((labels *labels*))
    (values (gethash label labels))))

(defmethod make-labeled-object (client input-stream (label integer))
  (declare (ignore client input-stream))
  (%make-labeled-object))

(defmethod labeled-object-state (client (object %labeled-object))
  (declare (ignore client))
  (values (%labeled-object-state object) (%labeled-object-object object)))

(defmethod finalize-labeled-object (client
                                    (labeled-object %labeled-object)
                                    object)
  (let ((new-state (case (%labeled-object-state labeled-object)
                     (:defined  :final)
                     (:circular :final/circular))))
    (setf (%labeled-object-state labeled-object) new-state
          (%labeled-object-object labeled-object) object)
    (when (eq new-state :final/circular)
      (fixup-graph client object))
    (values labeled-object new-state)))

(defmethod reference-labeled-object (client
                                     input-stream
                                     (labeled-object %labeled-object))
  (declare (ignore client input-stream))
  (ecase (%labeled-object-state labeled-object)
    ((:final :final/circular) ; Use final object, if it has already been stored
     (%labeled-object-object labeled-object))
    (:defined ; Else, use LABELED-OBJECT as placeholder, fix up later
     (setf (%labeled-object-state labeled-object) :circular)
     labeled-object)
    (:circular ; Same but without changing the state
     labeled-object)))

;;; Fixup
;;;
;;; We start from an object that was read and is known to contain
;;; placeholders for circular references. All placeholders have been
;;; finalized which means that they contain the respective object they
;;; stand in for. The fixup processing recursively traverses places in
;;; the object and its descendant objects and replaces placeholders
;;; with their respective final objects by modifying the containing
;;; places.

(defmethod fixup-graph (client root-object)
  (let ((seen (make-hash-table :test #'eq)))
    (fixup client root-object seen)))

(defmethod fixup :around (client object seen-objects)
  (declare (ignore client))
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (call-next-method)))

(defmethod fixup (client object seen-objects)
  (declare (ignore client object seen-objects))
  nil)

(defmacro fixup-case ((client value &key ((:state state-var) (gensym "STATE")))
                      (() &body normal-object-forms)
                      ((&optional object-var)
                       &body labeled-object-forms)
                      &optional ((() &body circular-labeled-object-forms) '(())))
  `(multiple-value-bind (,state-var ,@(when object-var `(,object-var)))
       (labeled-object-state ,client ,value)
     ;; If the state (in STATE-VAR) is null, VALUE is not a labeled
     ;; object. If the state is :FINAL or :FINAL/CIRCULAR, VALUE is a
     ;; labeled object that should be replaced by the object in
     ;; OBJECT-VAR. Otherwise (the state is :CIRCULAR), VALUE is a
     ;; labeled object for a different label as in #1=(1 #2=(#2# #1#))
     ;; when FIXUP-GRAPH is called for the object at #2=.
     (cond ((null ,state-var) ; not a labeled object
            ,@normal-object-forms)
           ((find ,state-var '(:final :final/circular)) ; finalized
            ,@labeled-object-forms)
           ,@(when circular-labeled-object-forms
               `((t ; not finalized
                  ,@circular-labeled-object-forms))))))

(defmacro fixup-place-using-value (client place current-value seen-objects)
  (alexandria:once-only (client)
    (alexandria:with-unique-names (object)
      `(fixup-case (,client ,current-value)
         (() (fixup ,client ,current-value ,seen-objects))
         ((,object) (setf ,place ,object))))))

(defmacro fixup-place (client place seen-objects)
  `(let ((current-value ,place))
     (fixup-place-using-value ,client ,place current-value ,seen-objects)))

(defmethod fixup (client (object cons) seen-objects)
  (fixup-place client (car object) seen-objects)
  (fixup-place client (cdr object) seen-objects))

(defmethod fixup (client (object array) seen-objects)
  ;; Fix up array elements unless the array element type indicates
  ;; that no fix up is required.
  (let ((element-type (array-element-type object)))
    (when (or (eq element-type 't) ; fast path
              (not (subtypep element-type '(or character number symbol))))
      (loop for i from 0 below (array-total-size object)
            do (fixup-place client (row-major-aref object i) seen-objects)))))

(defmethod fixup (client (object standard-object) seen-objects)
  (loop for slot-definition in (closer-mop:class-slots (class-of object))
        for name = (closer-mop:slot-definition-name slot-definition)
        when (slot-boundp object name)
          do (fixup-place client (slot-value object name) seen-objects)))

(defmethod fixup (client (object hash-table) seen-objects)
  (let ((key-changes '()))
    (maphash (lambda (key value)
               ;; If KEY has to be replaced, remove the entry for
               ;; KEY and VALUE (which is the only entry that may be
               ;; removed or changed according to the traversal
               ;; rules) and store the information for adding the
               ;; final key in KEY-CHANGES (since the traversal
               ;; rules forbid adding new entries). Fix up VALUE by
               ;; either fixing up the (gethash key object) place or
               ;; fixing up the value part of the queued KEY-CHANGES
               ;; entry.
               (fixup-case (client key)
                 (() ; not a labeled object
                  (fixup client key seen-objects)
                  (fixup-place-using-value
                   client (gethash key object) value seen-objects))
                 ((final-key) ; finalized labeled object
                  (remhash key object)
                  (let ((change (cons final-key value)))
                    (fixup-place-using-value
                     client (cdr change) value seen-objects)
                    (push change key-changes)))
                 (() ; labeled object not finalized
                  (fixup client value seen-objects))))
             object)
    (loop for (final-key . final-value) in key-changes
          do (setf (gethash final-key object) final-value))))
