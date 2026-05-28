;; Iterative fibonacci.
;; Exercises: while loops, break, multiple locals, comparisons.
;;
;; Equivalent C:
;;   int fib(int n) {
;;     int a = 0;
;;     int b = 1;
;;     int i = 0;
;;     while (i < n) {
;;       int tmp = b;
;;       b = a + b;
;;       a = tmp;
;;       i = i + 1;
;;     }
;;     return a;
;;   }
;;   int qbe_main() { return fib(10); }
;;
;;; #include <assert.h>
;;; extern int fib(int n);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(fib(0) == 0);
;;;     assert(fib(1) == 1);
;;;     assert(fib(6) == 8);
;;;     assert(fib(10) == 55);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) fib
    ((:type :i32) n)
    (:block
      (:declare (:type :i32) a 0)
      (:declare (:type :i32) b 1)
      (:declare (:type :i32) i 0)
      (:declare (:type :i32) tmp 0)
      (:while (:lt (:var i) (:var n))
        (:block
          (:set tmp (:var b))
          (:set b   (:add (:var a) (:var b)))
          (:set a   (:var tmp))
          (:set i   (:add (:var i) 1))))
      (:return (:var a))))

  (:function (:type :i32) qbe_main
    (:block
      (:return (:call fib 10)))))   ; expected exit code: 55
