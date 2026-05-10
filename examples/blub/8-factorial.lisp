;; Iterative factorial.
;; Exercises: while loop, multiplication accumulator, :le comparison.
;;
;; Equivalent C:
;;   int fact(int n) {
;;     int result = 1;
;;     int i = 1;
;;     while (i <= n) { result = result * i; i = i + 1; }
;;     return result;
;;   }
;;   int qbe_main() { return fact(5); }   /* = 120 */
;;
;;; #include <assert.h>
;;; extern int fact(int n);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(fact(0) == 1);
;;;     assert(fact(1) == 1);
;;;     assert(fact(5) == 120);
;;;     assert(fact(6) == 720);   /* > 255, only testable via assert, not exit code */
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) fact
    (:args ((:type :i32) n))
    (:block
      (:declare (:type :i32) result 1)
      (:declare (:type :i32) i 1)
      (:while (:le (:var i) (:var n))
        (:block
          (:set result (:mul (:var result) (:var i)))
          (:set i      (:add (:var i) 1))))
      (:return (:var result))))

  (:function (:type :i32) qbe_main (:args)
    (:block
      (:return (:call fact 5)))))   ; expected exit code: 120
