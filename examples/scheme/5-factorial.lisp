;; Scheme recursive factorial test
; expected exit code: 120
(:module
  (:define fact
    (:lambda (n)
      (:if (:le (:var n) 1)
           1
           (:mul (:var n) (:apply (:var fact) (:sub (:var n) 1))))))
  (:define main
    (:lambda ()
      (:apply (:var fact) 5))))
