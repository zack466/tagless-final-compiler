(:module
  (:global (:type :double) z 10.2)
  (:global (:type :double) a 1.5)
  (:function (:type :int) qbe_main (:args ((:type :int) x))
    (:block
      (:declare (:type :int) y 2)
      (:assign y (:add (:var x) (:var y)))))
  (:block
    (:declare (:type :int) z 1)
    (:declare (:type :int) y 2)
    (:declare (:type :int) y 3)
    (:assign y (:add (:var z) (:var y)))))
