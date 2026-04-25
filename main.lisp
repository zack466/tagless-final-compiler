(load "dev.lisp")
(in-package #:tagless-compiler)

(defparameter *hello-world-ast*
  '(:module
    ;; 1. Global data. Linkage omitted (nil), alignment omitted (nil).
    (:data (:global "str") nil nil
           (:data-item :b "\"hello world\"")
           (:data-item :b 0))

    ;; 2. Entry point. Using $main directly so we don't need a C shim.
    ;;    Linkage is a keyword (cleaner than a string).
    (:function (:global "qbe_main") :export :w ()
               (:block (:label "start")
                       (:call-assign (:temp "r") :w (:global "puts")
                                     (:call-arg :l (:global "str")))
                       (:assign (:temp "x") :w :add 10 20)
                       (:ret 0)))))

(format t "~a~%" (lower *qbe* *hello-world-ast*))

(build-qbe-ast *hello-world-ast*
               :out-name "hello_world"
               :runtime-c "runtime.c"
               :keep-temp-files t)

(uiop:run-program "./hello_world" :output *standard-output*)
