(asdf:defsystem #:tagless-compiler
  :description "A tagless-final compiler written in Common Lisp"
  :author "Zachary Huang"
  :license "BSD3"
  :depends-on (#:alexandria #:fset)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "interpreter")
                             (:file "qbe")))))

(asdf:defsystem #:tagless-compiler/tests
  :depends-on (#:tagless-compiler)
  :components ((:module "src"
                :serial t
                :components ((:file "tests")))))
