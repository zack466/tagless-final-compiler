;; Define package
(defpackage #:tagless-compiler
  (:use #:cl)
  (:export #:interpreter
           #:make-interpreter
           #:def-op
           #:lower
           #:*qbe*
           #:build-qbe-ast))

;; Define readtable for package
(in-package #:tagless-compiler)
(named-readtables:defreadtable tagless-compiler-syntax
  (:merge :standard)
  (:fuse fset2:fset-rereading-readtable))
