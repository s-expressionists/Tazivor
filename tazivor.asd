(asdf:defsystem :tazivor
  :description "A portable Common Lisp inspector"
  :license "MIT"
  :author "Tarn W. Burton"
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/yitzchak/Tazivor"
  :bug-tracker "https://github.com/yitzchak/Tazivor/issues"
  :depends-on (:closer-mop)
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "utilities")
                             (:file "interface")
                             (:file "object")
                             (:file "cons")
                             (:file "hash-table")
                             (:file "class")
                             (:file "standard-object")
                             (:file "structure-object")
                             (:file "symbol")
                             (:file "describe")
                             (:file "inspect")))))
