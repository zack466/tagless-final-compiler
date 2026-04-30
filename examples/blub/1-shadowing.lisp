(:module
  (:global (double z) 10.2)
  (:global (double a) 1.5)
  (:function int qbe_main ((int x))
             (:declare (int y) 2)
             (:assign y (:add (:var x) (:var y))))
  (:block
    (:declare (int z) 1)
    (:declare (int y) 2)
    (:declare (int y) 3)
    (:assign y (:add (:var z) (:var y)))))
