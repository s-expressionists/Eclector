(cl:in-package #:eclector.examples.highlight.render)

;;; Style
;;;
;;; A `style' specifies (a complete set of) attributes such as
;;; foreground color and background color of characters being
;;; output. `change-style' turns a base style and (possibly partially
;;; specified) style attributes into a complete `style'.

(deftype basic-color ()
  `(member :black :red :green :yellow :blue :magenta :cyan :white :default :gray))

(deftype foreground-color ()
  `(or basic-color
       (member :bright-red :bright-green :bright-yellow :bright-white)))

(defclass style ()
  ((%foreground :initarg :foreground
                :type    foreground-color
                :reader  foreground)
   (%background :initarg :background
                :type    basic-color
                :reader  background)
   (%italicp    :initarg :italicp
                :reader  italicp)
   (%boldp      :initarg :boldp
                :reader  boldp)
   (%underlinep :initarg :underlinep
                :reader  underlinep)))

(defmethod print-object ((object style) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~A ~A~:[~; italic~]~:[~; bold~]~:[~; underline~]"
            (foreground object) (background object)
            (italicp object) (boldp object) (underlinep object))))

(defun make-style (foreground background italicp boldp underlinep)
  (make-instance 'style :foreground foreground
                        :background background
                        :italicp    italicp
                        :boldp      boldp
                        :underlinep underlinep))

(defun change-style (current &key foreground
                                  background
                                  (italicp    nil italicp-suppliedp)
                                  (boldp      nil boldp-suppliedp)
                                  (underlinep nil underlinep-suppliedp))
  (let* ((current-foreground (foreground current))
         (current-background (background current))
         (current-italic     (italicp current))
         (current-bold       (boldp current))
         (current-underline  (underlinep current))
         (new-foreground     (or foreground
                                 current-foreground))
         (new-background     (or background
                                 current-background))
         (new-italic         (if italicp-suppliedp
                                 italicp
                                 current-italic))
         (new-bold           (if boldp-suppliedp
                                 boldp
                                 current-bold))
         (new-underline      (if underlinep-suppliedp
                                 underlinep
                                 current-underline)))
    (if (and (eql new-foreground current-foreground)
             (eql new-background current-background)
             (eql new-italic current-italic)
             (eql new-bold current-bold)
             (eql new-underline current-underline))
        current
        (make-style new-foreground new-background new-italic new-bold new-underline))))

(defmethod apply-style ((style style) stream)
  (format stream  "~C[~D;~D;~:[23~;3~];~:[22~;1~];~:[24~;4~]m"
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
          (italicp style)
          (boldp style)
          (underlinep style)))

;;; Default theme
;;;
;;; A theme associates classes of CST nodes with (partial)
;;; styles. `style-for-class' looks up (partial) style attributes for
;;; a given CST node class.

(defparameter *default-theme*
  '((root                       . ())

    (invalid                    . ())
    (definition                 . ())
    (reference                  . (:underlinep t))

    (skipped                    . (:foreground :gray))
    (line-comment               . (:foreground :gray))
    (block-comment              . (:foreground :gray))

    (symbol                     . (:foreground :green))
    (uninterned-symbol          . (:foreground :default))
    (keyword-symbol             . (:foreground :magenta))
    (interned-symbol            . (:foreground :default))
    (standard-symbol            . (:foreground :default :boldp t))
    (lambda-list-keyword-symbol . (:foreground :magenta))
    (two-package-markers        . (:background :red))

    (number                     . (:foreground :blue))

    (cons                       . ())
    (vector                     . (:foreground :yellow))
    (array                      . (:foreground :yellow))
    (character                  . (:foreground :yellow))
    (string                     . (:foreground :yellow))
    (pathname                   . (:foreground :yellow :underlinep t))

    (structure                  . ())

    (feature-expression         . (:background :yellow))

    (function                   . (:italicp t))
    (quote                      . (:background :gray))
    (quasiquote                 . (:background :black))
    (unquote                    . (:background :default))

    ;; Matching
    ((open  1)                  . (:foreground :blue    :underlinep t))
    ((open  2)                  . (:foreground :magenta :underlinep t))
    ((open  3)                  . (:foreground :green   :underlinep t))
    ((open  4)                  . (:foreground :yellow  :underlinep t))
    ((open  5)                  . (:foreground :cyan    :underlinep t))
    ((close 1)                  . (:foreground :blue    :underlinep t))
    ((close 2)                  . (:foreground :magenta :underlinep t))
    ((close 3)                  . (:foreground :green   :underlinep t))
    ((close 4)                  . (:foreground :yellow  :underlinep t))
    ((close 5)                  . (:foreground :cyan    :underlinep t))

    ;; Labeled objects
    ((definition 1)             . (:foreground :blue    :underlinep t))
    ((definition 2)             . (:foreground :magenta :underlinep t))
    ((definition 3)             . (:foreground :green   :underlinep t))
    ((definition 4)             . (:foreground :yellow  :underlinep t))
    ((definition 5)             . (:foreground :cyan    :underlinep t))
    ((reference  1)             . (:foreground :blue    :underlinep t))
    ((reference  2)             . (:foreground :magenta :underlinep t))
    ((reference  3)             . (:foreground :green   :underlinep t))
    ((reference  4)             . (:foreground :yellow  :underlinep t))
    ((reference  5)             . (:foreground :cyan    :underlinep t))

    ;; Error
    (error                      . (:foreground :red :underlinep t))))

(defmethod style-for-class ((class cons) (theme t))
  (destructuring-bind (which level) class
    (cond ((member which '(open close))
           (call-next-method (list which (1+ (mod (1- level) 5))) theme))
          ((member which '(definition reference))
           (call-next-method (list which (1+ (mod (1- level) 5))) theme))
          (t
           (call-next-method)))))

(defmethod style-for-class ((class t) (theme t))
  (multiple-value-bind (style foundp)
      (a:assoc-value theme class :test #'equal)
    (if foundp
        style
        (error "No style for ~A" class))))

;;; Client

(defclass ansi-text-client (stream-mixin
                            nesting-tracking-mixin)
  ((%theme       :initarg  :theme
                 :reader   theme
                 :initform *default-theme*)
   ;; State
   (%style-stack :accessor style-stack
                 :initform (list (make-style :default :default nil nil nil))
                 :documentation
                 "A stack of (complete) `style' instances."))
  (:default-initargs
   :stream (a:required-argument :stream)))

(defmethod style ((client ansi-text-client))
  (first (style-stack client)))

(defmethod push-style! (style (client ansi-text-client))
  (push style (style-stack client))
  (apply-style (style client) (stream client)))

(defmethod pop-style! ((client ansi-text-client))
  (pop (style-stack client))
  (apply-style (style client) (stream client)))

;;; Defaults

(defmethod style-class ((client ansi-text-client) (node t))
  ;; As a default, derive the style class from the class name of NODE.
  (let* ((name    (symbol-name (class-name (class-of node))))
         (trimmed (subseq name 0 (- (length name) (length "-node")))))
    (find-symbol trimmed (load-time-value '#.(package-name *package*)))))

(defmethod enter-node ((client ansi-text-client) (node t))
  (let* ((style-class    (style-class client node))
         (new-attributes (style-for-class style-class (theme client)))
         (new-style      (apply #'change-style (style client) new-attributes)))
    (push-style! new-style client)))

(defmethod leave-node ((client ansi-text-client) (node t))
  (pop-style! client))

(defmethod write-character ((client    ansi-text-client)
                            (position  t)
                            (character t)
                            (node      t))
  (write-char character (stream client)))

;;; Document

(defmethod enter-node ((client ansi-text-client) (node cst:root-node))
  ;; (fresh-line *standard-output*)
  ;; (utilities.print-tree:print-tree
  ;;  *standard-output* node
  ;;  (utilities.print-tree:make-node-printer
  ;;   (lambda (stream depth node)
  ;;     (declare (ignore depth))
  ;;     (princ node stream))
  ;;   nil
  ;;   #'cst:children))
  ;; (terpri)
  ;; (finish-output *standard-output*)
  (apply-style (style client) (stream client)))

(defmethod leave-node ((client ansi-text-client) (node cst:root-node))
  (finish-output (stream client)))

;;; Symbol

(defun name-of-packagep (string package)
  (or (string= string (package-name package))
      (member string (package-nicknames package) :test #'string=)))

(defmethod style-class ((client ansi-text-client)
                        (node   cst:interned-symbol-node))
  (cond ((cst:internp node)
         'two-package-markers)
        (t
         (call-next-method))))

;;; Errors
;;;
;;; `error-markup-mixin' applies the `error' style to input characters
;;; at which syntax errors have been recorded.
;;;
;;; `error-message-mixin' additionally inserts the corresponding error
;;; message after the invalid characters.

(defclass error-markup-mixin ()
  ())

(defmethod enter-errors ((client error-markup-mixin) (errors sequence))
  (let* ((style     (style-for-class 'error (theme client)))
         (new-style (apply #'change-style (style client) style)))
    (push-style! new-style client)))

(defmethod leave-errors ((client error-markup-mixin) (errors sequence))
  (pop-style! client))

(defclass error-message-mixin (error-markup-mixin)
  ())

(defmethod leave-errors ((client error-message-mixin) (errors sequence))
  (let ((stream (stream client)))
    (write-string "[" stream)
    (loop :for (error rest) :on errors
          :do (write-string (cst:message error) stream)
          :when rest
          :do (write-string "/" stream))
    (write-string "]" stream)))

;;; `nesting-highlighting-mixin'
;;;
;;; Highlights opening and closing delimiters of sequence-like nodes
;;; when they contain a given "point" position.

(defclass nesting-highlighting-mixin ()
  ((%point :initarg :point
           :reader  point)))

(defmethod write-character :around ((client    nesting-highlighting-mixin)
                                    (position  t)
                                    (character t)
                                    (node      cst:sequence-node))
  (let* ((children (cst:children node))
         (start    (cst:start node))
         (end      (cst:end node)))
    (if (<= start (point client) end)
        (let* ((child-start  (a:when-let ((child (first children)))
                               (cst:start child)))
               (child-end    (a:when-let ((child (a:lastcar children)))
                               (cst:end child)))

               (open-start?  (and (eql position start) (not (eql start child-start))))
               (open-end?    (or (and (not child-start) (eql position start))
                                 (eql (1+ position) child-start)))

               (close-start? (or (and (not child-end) (eql position (1- end)))
                                 (eql position child-end)))
               (close-end?   (and (eql position (1- end)) (not (eql end child-end)))))
          (when open-start?
            (let* ((name      `(open ,(nesting-depth client)))
                   (style     (style-for-class name (theme client)))
                   (new-style (apply #'change-style (style client) style)))
              (push-style! new-style client)))
          (when close-start?
            (let* ((name      `(close ,(nesting-depth client)))
                   (style     (style-for-class name (theme client)))
                   (new-style (apply #'change-style (style client) style)))
              (push-style! new-style client)))
          (call-next-method)
          (when open-end?
            (pop-style! client))
          (when close-end?
            (pop-style! client)))
        (call-next-method))))

(defmethod write-character :around ((client    nesting-highlighting-mixin)
                                    (position  t)
                                    (character t)
                                    (node      cst::definition-node))
  (let* ((children    (cst:children node))
         (start       (cst:start node))
         ; (end         (cst:end node))
         (child-start (a:when-let ((child (first children)))
                        (cst:start child))))
    (if (<= start (point client) child-start)
        (let* ((open-start? (and (eql position start) (not (eql start child-start))))
               (open-end?   (or (and (not child-start) (eql position start))
                                (eql (1+ position) child-start))))
          (when open-start?
            (let* ((label     (cst::label node))
                   (name      `(definition ,label))
                   (style     (style-for-class name (theme client)))
                   (new-style (apply #'change-style (style client) style)))
              (push-style! new-style client)))
          (call-next-method)
          (when open-end?
            (pop-style! client)))
        (call-next-method))))

(defmethod write-character :around ((client    nesting-highlighting-mixin)
                                    (position  t)
                                    (character t)
                                    (node      cst::reference-node))
  (call-next-method))
