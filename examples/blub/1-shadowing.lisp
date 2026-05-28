;; Variable shadowing test: pass 1 (alpha-rename) detects that 'x' is
;; declared twice in the same block and renames the second to a fresh name.
;;
;; Step by step (after pass 0 desugars declares, before pass 1):
;;   declare x              x = 10
;;   declare y              y = x       (= 10, reads first x)
;;   declare x (shadows!)   x.1 = 20   ('x' in scope now means x.1)
;;   y = y + x              y = 10 + 20 = 30  (x resolves to x.1)
;;   return y               = 30
;;
;; Equivalent C (after alpha-rename):
;;   int shadow_test() {
;;     int x  = 10;
;;     int y  = x;      /* = 10 */
;;     int x1 = 20;     /* shadows x */
;;     y = y + x1;      /* = 30 */
;;     return y;
;;   }
;;   int qbe_main() { return shadow_test(); }   /* = 30 */
;;
;;; #include <assert.h>
;;; extern int shadow_test();
;;; extern int qbe_main();
;;; int main() {
;;;     assert(shadow_test() == 30);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) shadow-test
    (:block
      (:declare (:type :i32) x 10)
      (:declare (:type :i32) y (:var x))
      (:declare (:type :i32) x 20)
      (:set y (:add (:var y) (:var x)))
      (:return (:var y))))

  (:function (:type :i32) qbe_main
    (:block
      (:return (:call shadow-test)))))   ; expected exit code: 30
