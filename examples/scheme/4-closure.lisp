;; Basic Scheme closure test
; expected exit code: 15
(:module
  (:define make-adder
    (:lambda (x)
      (:lambda (y)
        (:add (:var x) (:var y)))))
  (:define main
    (:lambda ()
      (:let ((add5 (:apply (:var make-adder) 5)))
        (:apply (:var add5) 10)))))
