;; A function that adds two integers and returns the result.
;; Exercises: parameter spilling, variable declaration, assignment,
;; arithmetic expression lowering, and return.
;;
;; Equivalent C:
;;   int add(int a, int b) { return a + b; }
;;   int qbe_main() {
;;     int result;
;;     result = add(10, 32);
;;     return result;
;;   }
;;
;;; #include <assert.h>
;;; extern int add(int a, int b);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(add(1, 2) == 3);
;;;     assert(add(10, 32) == 42);
;;;     assert(add(-5, 5) == 0);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) add
    ((:type :i32) a) ((:type :i32) b)
    (:block
      (:return (:add (:var a) (:var b)))))

  (:function (:type :i32) qbe_main
    (:block
      (:declare (:type :i32) result 0)
      (:set result (:call add 10 32))
      (:return (:var result)))))   ; expected exit code: 42
