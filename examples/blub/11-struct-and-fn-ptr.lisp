;; Combined: apply a function pointer to a struct passed by reference.
;; Exercises: (:defstruct), (:set (:. struct field) val), (:fn-ptr),
;; (:call fn-expr args...) with typed function pointers, and passing both
;; a function pointer and a struct pointer as arguments to a higher-order function.
;;
;; Equivalent C:
;;   struct Point { int x; int y; };
;;   int get_x(struct Point *p) { return p->x; }
;;   int get_y(struct Point *p) { return p->y; }
;;   int apply_to_point(int (*fn)(struct Point *), struct Point *p) { return fn(p); }
;;   int qbe_main() {
;;     struct Point pt; pt.x = 10; pt.y = 32;
;;     return apply_to_point(get_x, &pt) + apply_to_point(get_y, &pt);  /* = 42 */
;;   }
;;
;;; #include <assert.h>
;;; struct Point { int x; int y; };
;;; extern int get_x(struct Point *p);
;;; extern int get_y(struct Point *p);
;;; extern int apply_to_point(void *fn, struct Point *p);
;;; extern int qbe_main();
;;; int main() {
;;;     struct Point p = {5, 7};
;;;     assert(get_x(&p) == 5);
;;;     assert(get_y(&p) == 7);
;;;     assert(apply_to_point(get_x, &p) == 5);
;;;     assert(apply_to_point(get_y, &p) == 7);
;;;     struct Point q = {10, 32};
;;;     assert(apply_to_point(get_x, &q) + apply_to_point(get_y, &q) == 42);
;;;     return qbe_main();
;;; }
(:module
  (:defstruct point ((:type :i32) x) ((:type :i32) y))

  (:function (:type :i32) get-x
    ((:type (:pointer (:type (:struct point)))) p)
    (:block (:return (:. (:deref (:var p)) x))))

  (:function (:type :i32) get-y
    ((:type (:pointer (:type (:struct point)))) p)
    (:block (:return (:. (:deref (:var p)) y))))

  (:function (:type :i32) apply-to-point
    ((:type (:fn (:type :i32) (:type (:pointer (:type (:struct point)))))) fn)
    ((:type (:pointer (:type (:struct point)))) p)
    (:block (:return (:call (:var fn) (:var p)))))

  (:function (:type :i32) qbe_main
    (:block
      (:declare (:type (:struct point)) pt)
      (:set (:. (:var pt) x) 10)
      (:set (:. (:var pt) y) 32)
      (:declare (:type :i32) r 0)
      (:set r (:add (:call apply-to-point (:fn-ptr get-x) (:addr-of (:var pt)))
                       (:call apply-to-point (:fn-ptr get-y) (:addr-of (:var pt)))))
      (:return (:var r)))))   ; expected exit code: 42
