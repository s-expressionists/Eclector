(defsystem "eclector"
  :author "Robert Strandh"
  :maintainer "Jan Moringen"
  :license "BSD"
  :version (:read-file-form "version.sexp")

  :depends-on ("alexandria"
               "concrete-syntax-tree"
               "sicl-simple-readtable"
               "closer-mop")

  :components ((:module "Reader"
                :pathname "Code/Reader"
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
