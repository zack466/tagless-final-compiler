;; Deeply nested arithmetic to exercise pass 4 normalization.
;; Computes: (a*b + c*d) - 1 = (3*7 + 4*5) - 1 = (21 + 20) - 1 = 40
;;
;; Pass 4 must extract (:mul a b) and (:mul c d) as temporaries before
;; the :add, and then the :add before the :sub.
;;
;; Equivalent C:
;;   int compute(int a, int b, int c, int d) {
;;     return (a*b + c*d) - 1;
;;   }
;;   int qbe_main() { return compute(3, 7, 4, 5); }   /* = 40 */
;;
;;; #include <assert.h>
;;; extern int compute(int a, int b, int c, int d);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(compute(1, 1, 1, 1) == 1);   /* 1 + 1 - 1 = 1 */
;;;     assert(compute(3, 7, 4, 5) == 40);  /* 21 + 20 - 1 = 40 */
;;;     assert(compute(2, 3, 4, 5) == 25);  /* 6 + 20 - 1 = 25 */
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) compute
    ((:type :i32) a) ((:type :i32) b)
    ((:type :i32) c) ((:type :i32) d)
    (:block
      (:return (:sub (:add (:mul (:var a) (:var b))
                           (:mul (:var c) (:var d)))
                     1))))

  (:function (:type :i32) qbe_main
    (:block
      (:return (:call compute 3 7 4 5)))))   ; expected exit code: 40
