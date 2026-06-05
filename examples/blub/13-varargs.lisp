;; Variadic functions: sum_n(n, ...) returns the sum of n i32 args.
;;
;; Equivalent C:
;;   int sum_n(int n, ...) {
;;       va_list ap;
;;       va_start(ap, n);
;;       int total = 0;
;;       int i = 0;
;;       while (i < n) { total = total + va_arg(ap, int); i = i + 1; }
;;       return total;
;;   }
;;
;;; #include <assert.h>
;;; extern int sum_n(int n, ...);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(sum_n(3, 10, 20, 30) == 60);
;;;     assert(sum_n(1, 7) == 7);
;;;     assert(sum_n(0) == 0);
;;;     assert(sum_n(5, 1, 2, 3, 4, 5) == 15);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) sum_n
    ((:type :i32) n) (:varargs)
    (:block
      (:declare (:type :valist) ap)
      (:vastart ap)
      (:declare (:type :i32) total 0)
      (:declare (:type :i32) i 0)
      (:while (:lt (:var i) (:var n))
        (:block
          (:set total (:add (:var total) (:vaarg ap (:type :i32))))
          (:set i (:add (:var i) 1))))
      (:return (:var total))))

  (:function (:type :i32) qbe_main
    (:block
      ;; sum_n(4, 5, 10, 15, 20) = 50
      (:return (:call sum_n 4 (:varargs) 5 10 15 20)))))   ; expected exit code: 50
