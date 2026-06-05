;; Basic Scheme pairs test
; expected exit code: 35
(:module
  (:define main
    (:lambda ()
      (:let ((p (:cons 35 42)))
        (:car (:var p))))))
