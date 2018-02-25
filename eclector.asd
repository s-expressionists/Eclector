(defsystem "eclector"
  :author ("Robert Strandh"
           "Jan Moringen")
  :maintainer "Jan Moringen"
  :license "BSD"
  :version (:read-file-form "version.sexp")

  :depends-on ("alexandria"
               "concrete-syntax-tree"
               "closer-mop")

  :components ((:module "Readtable"
                :pathname "Code/Readtable"
                :serial t
                :components ((:file "packages")
                             (:file "generic-functions")))

               (:module "Simple-Readtable"
                :pathname "Code/Readtable/Simple"
                :depends-on ("Readtable")
                :serial t
                :components ((:file "packages")
                             (:file "readtable")
                             (:file "methods")))

               (:module "Reader"
                :pathname "Code/Reader"
                :depends-on ("Readtable")
                :serial t
                :components ((:file "packages")
                             (:file "more-variables")
                             (:file "additional-conditions")
                             (:file "utilities")
                             (:file "tokens")
                             (:file "read-common")
                             (:file "read-cst")
                             (:file "read")
                             (:file "macro-functions")
                             (:file "init")
                             (:file "quasiquote-macro")
                             (:file "fixup")))

               (:static-file "README.md")
               (:static-file "LICENSE-BSD"))

  :in-order-to ((test-op (test-op "eclector/test"))))

(defsystem "eclector/test"
  :depends-on ("eclector"
               (:version "fiveam" "1.4"))

  :components ((:module "Test"
                :components ((:file "packages")))

               (:module "Readtable"
                :pathname "Test/Readtable"
                :depends-on ("Test")
                :serial t
                :components ((:file "packages")))

               (:module "Reader"
                :pathname "Test/Reader"
                :depends-on ("Test")
                :serial t
                :components ((:file "packages")

                             (:file "gen-quasiquote")

                             (:file "tokens")
                             (:file "macro-functions")
                             (:file "quasiquote-macro"))))

  :perform (test-op (operation component)
             (uiop:symbol-call '#:eclector.test '#:run-tests)))
