;; Iterative sum from 1 to N.
;; Exercises: while loop with accumulator, :le comparison.
;;
;; Equivalent C:
;;   int sum_to(int n) {
;;     int i = 1;
;;     int acc = 0;
;;     while (i <= n) { acc = acc + i; i = i + 1; }
;;     return acc;
;;   }
;;   int qbe_main() { return sum_to(10); }   /* = 55 */
;;
;;; #include <assert.h>
;;; extern int sum_to(int n);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(sum_to(0)  == 0);
;;;     assert(sum_to(1)  == 1);
;;;     assert(sum_to(5)  == 15);
;;;     assert(sum_to(10) == 55);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) sum-to
    (:args ((:type :i32) n))
    (:block
      (:declare (:type :i32) i 1)
      (:declare (:type :i32) acc 0)
      (:while (:le (:var i) (:var n))
        (:block
          (:set acc (:add (:var acc) (:var i)))
          (:set i   (:add (:var i) 1))))
      (:return (:var acc))))

  (:function (:type :i32) qbe_main (:args)
    (:block
      (:return (:call sum-to 10)))))   ; expected exit code: 55
