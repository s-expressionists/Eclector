(defsystem "eclector.syntax-extensions"
  :description "A collection of extensions to the Common Lisp syntax."
  :license     "BSD"
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("eclector")

  :components  ((:module "syntax-extensions"
                 :pathname "code/syntax-extensions"
                 :components ((:file "extended-package-prefix")
                              (:file "s-expression-comment")
                              (:file "rational-float"))))

  :in-order-to ((test-op (test-op "eclector.syntax-extensions/test"))))

(defsystem "eclector.syntax-extensions/test"
  :depends-on  ("eclector.syntax-extensions"
                "eclector-concrete-syntax-tree"
                (:version "fiveam" "1.4"))

  :components  ((:module "syntax-extensions"
                 :pathname "test/syntax-extensions"
                 :serial t
                 :components ((:file "package")
                              (:file "extended-package-prefix")
                              (:file "s-expression-comment")
                              (:file "rational-float"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:eclector.syntax-extensions.test '#:run-tests)))
