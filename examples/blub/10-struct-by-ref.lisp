;; Struct by reference: pass a pointer to a struct and access its fields.
;; Exercises: (:defstruct), (:set (:. struct field) val), (:addr-of), (:deref), and
;; struct field access via (:. (:deref ptr) field).
;;
;; Equivalent C:
;;   struct Point { int x; int y; };
;;   int sum_fields(struct Point *p) { return p->x + p->y; }
;;   int qbe_main() {
;;     struct Point pt;
;;     pt.x = 3; pt.y = 4;
;;     return sum_fields(&pt);   /* = 7 */
;;   }
;;
;;; #include <assert.h>
;;; struct Point { int x; int y; };
;;; extern int sum_fields(struct Point *p);
;;; extern int qbe_main();
;;; int main() {
;;;     struct Point a = {3, 4};
;;;     assert(sum_fields(&a) == 7);
;;;     struct Point b = {10, 32};
;;;     assert(sum_fields(&b) == 42);
;;;     struct Point zero = {0, 0};
;;;     assert(sum_fields(&zero) == 0);
;;;     return qbe_main();
;;; }
(:module
  (:defstruct point ((:type :i32) x) ((:type :i32) y))

  (:function (:type :i32) sum-fields
    (:args ((:type (:pointer (:type (:struct point)))) p))
    (:block
      (:return (:add (:. (:deref (:var p)) x)
                     (:. (:deref (:var p)) y)))))

  (:function (:type :i32) qbe_main (:args)
    (:block
      (:declare (:type (:struct point)) pt)
      (:set (:. (:var pt) x) 3)
      (:set (:. (:var pt) y) 4)
      (:return (:call sum-fields (:addr-of (:var pt)))))))   ; expected exit code: 7
