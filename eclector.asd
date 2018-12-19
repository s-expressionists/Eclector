(defsystem "eclector"
  :description "A portable, extensible Common Lisp reader."
  :license     "BSD"
  :author      ("Robert Strandh"
                "Jan Moringen")
  :maintainer  "Jan Moringen"

  :version     (:read-file-form "version.sexp")
  :depends-on  ("alexandria"
                "closer-mop"
                "acclimation")

  :components  ((:module "base"
                 :pathname "code/base"
                 :components ((:file "package")
                              (:file "conditions")))

                (:module "readtable"
                 :pathname "code/readtable"
                 :depends-on ("base")
                 :serial t
                 :components ((:file "package")
                              (:file "variables")
                              (:file "conditions")
                              (:file "condition-reporters-english")
                              (:file "generic-functions")))

                (:module "simple-readtable"
                 :pathname "code/readtable/simple"
                 :depends-on ("base"
                              "readtable")
                 :serial t
                 :components ((:file "package")
                              (:file "readtable")
                              (:file "methods")))

                (:module "reader"
                 :pathname "code/reader"
                 :depends-on ("base"
                              "readtable")
                 :serial t
                 :components ((:file "package")
                              (:file "generic-functions")
                              (:file "more-variables")
                              (:file "additional-conditions")
                              (:file "condition-reporters-english")
                              (:file "utilities")
                              (:file "tokens")
                              (:file "read-common")
                              (:file "read")
                              (:file "macro-functions")
                              (:file "init")
                              (:file "quasiquote-macro")
                              (:file "fixup")))

                (:module "parse-result"
                 :pathname "code/parse-result"
                 :depends-on ("reader")
                 :serial t
                 :components ((:file "package")
                              (:file "client")
                              (:file "generic-functions")
                              (:file "read")))

                (:static-file "README.md")
                (:static-file "LICENSE"))

  :in-order-to ((test-op (test-op "eclector/test"))))

(defsystem "eclector/test"
  :depends-on  ("eclector"
                (:version "fiveam" "1.4"))

  :components  ((:module "test"
                 :components ((:file "package")))

                (:module "readtable"
                 :pathname "test/readtable"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")))

                (:module "reader"
                 :pathname "test/reader"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")

                              (:file "gen-quasiquote")

                              (:file "tokens")
                              (:file "read")
                              (:file "macro-functions")
                              (:file "quasiquote-macro")
                              (:file "fixup")

                              (:file "readtable")

                              (:file "client")))

                (:module "parse-result"
                 :pathname "test/parse-result"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")
                              (:file "read"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:eclector.test '#:run-tests)))
