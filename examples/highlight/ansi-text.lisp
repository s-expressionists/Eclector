(cl:in-package #:eclector.examples.highlight)

;;; Style

(defclass style ()
  ((%foreground :initarg :foreground
                :reader  foreground)
   (%background :initarg :background
                :reader  background)
   (%underlinep :initarg :underlinep
                :reader  underlinep)))

(defmethod print-object ((object style) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~A ~A~:[~; underline~]"
            (foreground object)
            (background object)
            (underlinep object))))

(defun make-style (foreground background underlinep)
  (make-instance 'style :foreground foreground
                        :background background
                        :underlinep underlinep))

(defun change-style (current &key foreground
                                  background
                                  (underlinep nil underlinep-suppliedp))
  (let* ((current-foreground (foreground current))
         (current-background (background current))
         (current-underline  (underlinep current))
         (new-foreground     (or foreground
                                 current-foreground))
         (new-background     (or background
                                 current-background))
         (new-underline      (if underlinep-suppliedp
                                 underlinep
                                 current-underline)))
    (if (and (eql new-foreground current-foreground)
             (eql new-background current-background)
             (eql new-underline current-underline))
        current
        (make-style new-foreground new-background new-underline))))

;;;

(defparameter *theme*
  '((cst                             . ())

    (skipped-node                    . (:foreground :gray))
    (line-comment-node               . (:foreground :gray))

    (symbol-node                     . (:foreground :green))
    (uninterned-symbol-node          . (:foreground :default))
    (keyword-symbol-node             . (:foreground :magenta))
    (interned-symbol-node            . (:foreground :default))
    (lambda-list-keyword-symbol-node . (:foreground :magenta))
    (two-package-markers             . (:background :red))

    (number-node                     . (:foreground :blue))

    (cons                            . ())
    (vector                          . (:foreground :yellow))
    (array-node                      . (:foreground :yellow))
    (character-node                  . (:foreground :yellow))
    (string                          . (:foreground :yellow))
    (pathname-node                   . (:foreground :yellow :underlinep t))

    (function-node                   . (:background :blue))
    (quote-node                      . (:background :gray))
    (quasiquote-node                 . (:background :black))
    (unquote-node                    . (:background :default))

    (feature-expression-node         . (:background :yellow))))

(defmethod style-for-class ((class t) (theme t))
  (multiple-value-bind (style foundp) (a:assoc-value *theme* class)
    (if foundp
        style
        (error "No style for ~A" class))))

;;; Client

(defclass ansi-text-client ()
  ((%stream      :initarg  :stream
                 :reader   stream)
   (%style-stack :accessor style-stack
                 :initform (list (make-style :default :default nil))))
  (:default-initargs
   :stream (a:required-argument :stream)))

(defmethod style ((client ansi-text-client))
  (first (style-stack client)))

(defmethod push-style! (style (client ansi-text-client))
  (push style (style-stack client))
  (apply-style client))

(defmethod pop-style! ((client ansi-text-client))
  (pop (style-stack client))
  (apply-style client))

(defmethod apply-style ((client ansi-text-client))
  (let ((style (style client)))
    (format (stream client) "~C[~D;~D;~:[24~;4~]m"
            #\Escape
            (ecase (background style)
              (:black    40)
              (:red      41)
              (:green    42)
              (:yellow   43)
              (:blue     44)
              (:magenta  45)
              (:cyan     46)
              (:white    47)
              (:default  49)
              (:gray    100))
            (ecase (foreground style)
              (:black         30)
              (:red           31)
              (:green         32)
              (:yellow        33)
              (:blue          34)
              (:magenta       35)
              (:cyan          36)
              (:white         37)
              (:default       39)
              (:gray          90)
              (:bright-red    91)
              (:bright-green  92)
              (:bright-yellow 93)
              (:bright-white  97))
            (underlinep style))))

;;; Defaults

(defmethod style-class ((client ansi-text-client) (node t))
  (class-name (class-of node)))

(defmethod enter-node ((client ansi-text-client) (node t))
  (let* ((style-class (a:ensure-car (style-class client node)))
         (new-style   (style-for-class style-class :theme))
         (new-style   (apply #'change-style (style client) new-style)))
    (push-style! new-style client)))

(defmethod leave-node ((client ansi-text-client) (node t))
  (pop-style! client))

(defmethod write-character ((client    ansi-text-client)
                            (position  t)
                            (character t)
                            (node      t))
  (write-char character (stream client)))

;;; Document

(defmethod enter-node :before ((client ansi-text-client) (node cst))
  (apply-style client))

(defmethod leave-node :after ((client ansi-text-client) (node cst))
  (finish-output (stream client)))

;;; Nothing to do?

;;; Skipped

(defmethod style-class ((client ansi-text-client) (node skipped-node))
  'comment)

(defmethod style-class ((client ansi-text-client) (node block-comment-node))
  (list* 'block-comment (call-next-method)))

(defmethod style-class ((client ansi-text-client) (node line-comment-node))
  (list* 'line-comment (call-next-method)))

;;; Quote

;;; Quasiquote

(defvar *quasiquote-depth* 0)

(defmethod enter-node ((client ansi-text-client) (node quasiquote-node))
  (incf *quasiquote-depth*)
  (call-next-method))

(defmethod leave-node ((client ansi-text-client) (node quasiquote-node))
  (call-next-method)
  (decf *quasiquote-depth*))

(defmethod enter-node ((client ansi-text-client) (node unquote-node))
  (decf *quasiquote-depth*)
  (call-next-method))

(defmethod leave-node ((client ansi-text-client) (node unquote-node))
  (call-next-method)
  (incf *quasiquote-depth*))

;;; Number

(defmethod style-class ((client ansi-text-client) (node number-node))
  'number)

;;; Symbol

#+later (defmethod leave-node :before ((client t) (node interned-symbol-node) (stream t))
  (when (intern? node)
    (format stream "<span class=\"message\">~A</span>" "Do not use unexported symbols.")))

#+later (defmethod style-class ((client t) (node interned-symbol-node)) ; TODO return list and use APPEND method combination?
  (if (intern? node)
      (list* "two-package-markers" (a:ensure-list (call-next-method)))
      (call-next-method)))

#+later (defun name-of-package? (string package)
  (or (string= string (package-name package))
      (member string (package-nicknames package) :test #'string=)))

;;; Sequence

(defvar *nesting-depth* 0)

(defmethod enter-node ((client ansi-text-client) (node sequence-node))
  (incf *nesting-depth*)
  (call-next-method))

(defmethod leave-node ((client ansi-text-client) (node sequence-node))
  (call-next-method)
  (decf *nesting-depth*))

#+no (defmethod write-character ((client    ansi-text-client)
                            (position  t)
                            (character t)
                            (node      sequence-node)
                            (stream    t))

  (let* ((start        (start node))
         (end          (end node))
         (child-start  (a:when-let ((child (first (children node))))
                         (start child)))
         (child-end    (a:when-let ((child (a:lastcar (children node))))
                         (end child)))

         (open-start?  (and (eql position start) (not (eql start child-start))))
         (open-end?    (or (and (not child-start) (eql position start))
                           (eql (1+ position) child-start)))

         (close-start? (or (and (not child-end) (eql position (1- end)))
                           (eql position child-end)))
         (close-end?   (and (eql position (1- end)) (not (eql end child-end)))))

    (when open-start?
      (<span stream "open"))
    (when close-start?
      (<span stream "close"))
    (call-next-method)
    (when open-end?
      (/span stream))
    (when close-end?
      (/span stream))))

;;; String

(defmethod style-class ((client ansi-text-client) (node string-node))
  'string)

;;; Vector

(defmethod style-class ((client ansi-text-client) (node vector-node))
  'vector)

;;; Cons

(defmethod style-class ((client ansi-text-client) (node cons-node))
  'cons)

;;; Errors

(defmethod enter-errors ((client ansi-text-client) (errors sequence))
  (format (stream client) "~C[4;31m" #\Escape))

(defmethod leave-errors ((client ansi-text-client) (errors sequence))
  (let ((stream (stream client)))
    (write-string "[" stream)
    (loop :for (error rest) :on errors
          :do (write-string (message error) stream)
          :when rest
          :do (write-string "/" stream))
    (write-string "]" stream)
    (format stream "~C[4;39m" #\Escape)))
