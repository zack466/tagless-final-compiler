(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

(defparameter *hello-world*
  (first (read-program "qbe/hello_world.lisp")))

(format t "~a~%" (lower *qbe* *hello-world*))

(build-qbe-ast *hello-world*
               :out-name "hello_world"
               :runtime-c "runtime.c"
               :keep-temp-files t)

(uiop:run-program "./hello_world" :output *standard-output*)
