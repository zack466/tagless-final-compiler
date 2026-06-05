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
                             (:file "grammar")
                             (:file "meta-grammar")
                             (:file "interpreter")
                             (:file "example")
                             (:file "qbe")
                             (:file "blub")
                             (:file "scheme")))))

(asdf:defsystem #:tagless-compiler/tests
  :depends-on (#:tagless-compiler)
  :components ((:module "test"
                :serial t
                :components ((:file "harness")
                             (:file "tests")
                             (:file "test-grammar")
                             (:file "test-meta-grammar")
                             (:file "test-cst")
                             (:file "test-context")
                             (:file "test-propagation")
                             (:file "test-match")
                             (:file "test-blub-passes")
                             (:file "test-scheme-passes")))))
