(asdf:defsystem #:tagless-compiler
  :description "A tagless-final compiler written in Common Lisp"
  :author "Zachary Huang"
  :license "BSD3"
  ;; We group our files inside a module that matches the folder name
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "interpreter")
                             (:file "qbe")))))
