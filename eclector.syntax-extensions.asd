(defsystem "eclector.syntax-extensions"
  :description "A collection of extensions to the Common Lisp syntax."
  :license     "BSD"
  :author      "Jan Moringen"
  :maintainer  "Jan Moringen"

  :version     (:read-file-form "version.sexp")
  :depends-on  ("eclector")

  :components  ((:module "extensions"
                 :pathname "code/extensions"
                 :components ((:file "object-in-package"))))

  :in-order-to ((test-op (test-op "eclector.syntax-extensions/test"))))

(defsystem "eclector.syntax-extensions/test"
  :depends-on  ("eclector.syntax-extensions"
                (:version "fiveam" "1.4"))

  :components  ((:module "extensions"
                 :pathname "test/extensions"
                 :serial t
                 :components ((:file "package")
                              (:file "object-in-package"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:eclector.syntax-extensions.test '#:run-tests)))
