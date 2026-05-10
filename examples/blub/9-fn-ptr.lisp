;; Function pointer: pass a function via pointer and call it indirectly.
;; Exercises: (:fn-ptr name), (:call fn-expr args...) with indirect callee,
;; and (:type (:fn ret param...)) typed function pointer parameters.
;;
;; Equivalent C:
;;   int double_it(int x)           { return x * 2; }
;;   int triple(int x)              { return x * 3; }
;;   int apply(void *fn, int x)     { return ((int(*)(int))fn)(x); }
;;   int qbe_main() {
;;     return apply(double_it, 5) + apply(triple, 5);   /* = 25 */
;;   }
;;
;;; #include <assert.h>
;;; extern int double_it(int x);
;;; extern int triple(int x);
;;; extern int apply(void *fn, int x);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(double_it(4) == 8);
;;;     assert(triple(4)    == 12);
;;;     assert(apply(double_it, 3) == 6);
;;;     assert(apply(triple,   3) == 9);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) double-it
    (:args ((:type :i32) x))
    (:block (:return (:mul (:var x) 2))))

  (:function (:type :i32) triple
    (:args ((:type :i32) x))
    (:block (:return (:mul (:var x) 3))))

  (:function (:type :i32) apply
    (:args ((:type (:fn (:type :i32) (:type :i32))) fn) ((:type :i32) x))
    (:block (:return (:call (:var fn) (:var x)))))

  (:function (:type :i32) qbe_main (:args)
    (:block
      (:declare (:type :i32) r 0)
      (:set r (:add (:call apply (:fn-ptr double-it) 5)
                    (:call apply (:fn-ptr triple) 5)))
      (:return (:var r)))))   ; expected exit code: 25
