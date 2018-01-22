(defsystem "eclector"
  :author "Robert Strandh"
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
               (:static-file "LICENSE-BSD")))
