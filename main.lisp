(load "dev.lisp")
(in-package #:tagless-compiler)

(defparameter *hello-world-ast*
  '(:module
    
    ;; 1. Global Data (Strings are just data values now)
    (:data (:global "str") nil nil
           (:data-item :b "\"hello world\"")
           (:data-item :b 0))
    
    ;; 2. Function
    (:function (:global "qbe_main") "export" :w ()
               (:block (:label "start")
                       ;; Call assignment 
                       (:call-assign (:temp "r") :w (:global "puts") 
                                     (:call-arg :l (:global "str")))
                       
                       ;; Math example using strictly keywords
                       (:assign (:temp "x") :w :add 10 20)
                       
                       (:ret 0)))))

(format t "~a~%" (lower *qbe* *hello-world-ast*))

(build-qbe-ast *hello-world-ast* :out-name "hello_world" 
               :runtime-c "runtime.c" 
               :keep-temp-files t)

(uiop:run-program "./hello_world" :output *standard-output*)
