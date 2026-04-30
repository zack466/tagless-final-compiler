(asdf:defsystem #:tagless-compiler
  :description "A tagless-final compiler written in Common Lisp"
  :author "Zachary Huang"
  :license "BSD3"
  :depends-on (#:alexandria
               #:fset
               #:eclector
               #:eclector-concrete-syntax-tree
               #:cl-ansi-text
               #:cl-ppcre
               #:trivial-garbage
               )
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "formatting")
                             (:file "util")
                             (:file "source")
                             (:file "interpreter")
                             (:file "example")
                             (:file "qbe")))))

(asdf:defsystem #:tagless-compiler/tests
  :depends-on (#:tagless-compiler)
  :components ((:module "test"
                :serial t
                :components ((:file "tests")
                             (:file "test-cst")
                             (:file "test-context")
                             (:file "test-propagation")
                             (:file "test-match")
                             (:file "test-qbe")))))
