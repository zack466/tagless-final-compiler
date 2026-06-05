;; Basic Scheme arithmetic test
; expected exit code: 25
(:module
  (:define main
    (:lambda ()
      (:mul (:add 2 3) 5))))
