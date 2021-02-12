(cl:defpackage #:eclector.syntax-extensions.object-in-package
  (:use
   #:cl)

  (:export
   #:object-in-package-syntax-mixin))

(cl:in-package #:eclector.syntax-extensions.object-in-package)

(defclass object-in-package-syntax-mixin ()
  ())

(defmethod eclector.reader:check-symbol-token
    ((client                    object-in-package-syntax-mixin)
     (input-stream              t)
     (token                     t)
     (escape-ranges             t)
     (position-package-marker-1 integer)
     (position-package-marker-2 integer))
  (let ((length (length token)))
    (if (and (= position-package-marker-2 (1- length))
             (= position-package-marker-1 (1- position-package-marker-2)))
        (values token position-package-marker-1 position-package-marker-2)
        (call-next-method))))

(defmethod eclector.reader:interpret-symbol-token
    ((client                    object-in-package-syntax-mixin)
     (input-stream              t)
     (token                     t)
     (position-package-marker-1 integer)
     (position-package-marker-2 integer))
  (let ((length (length token)))
    (if (and (= position-package-marker-2 (1- length))
             (= position-package-marker-1 (1- position-package-marker-2)))
        (eclector.reader:call-with-current-package
         client
         (lambda ()
           (eclector.reader:read input-stream t nil t))
         (subseq token 0 position-package-marker-1))
        (call-next-method))))
