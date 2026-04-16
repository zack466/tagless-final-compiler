(defpackage #:tagless-compiler
  (:use #:cl)
  (:export #:interpreter
           #:make-interpreter
           #:def-op
           #:lower
           #:*qbe*
           #:build-qbe-ast))
