;; Bitwise shift operations: shl (left shift) and shr (logical right shift).
;;
;; Equivalent C:
;;   int shl_op(int x, int n) { return x << n; }
;;   int shr_op(int x, int n) { return (unsigned)x >> n; }
;;
;;; #include <assert.h>
;;; extern int shl_op(int x, int n);
;;; extern int shr_op(int x, int n);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(shl_op(1, 0) == 1);
;;;     assert(shl_op(1, 4) == 16);
;;;     assert(shl_op(3, 2) == 12);
;;;     assert(shr_op(16, 4) == 1);
;;;     assert(shr_op(256, 3) == 32);
;;;     assert(shr_op(1, 0) == 1);
;;;     return qbe_main();
;;; }
(:module
  (:function (:type :i32) shl_op
    ((:type :i32) x) ((:type :i32) n)
    (:block
      (:return (:shl (:var x) (:var n)))))

  (:function (:type :i32) shr_op
    ((:type :i32) x) ((:type :i32) n)
    (:block
      (:return (:shr (:var x) (:var n)))))

  (:function (:type :i32) qbe_main
    (:block
      ;; (1 << 5) | (255 >> 3) = 32 | 31 = 63
      (:return (:or (:shl 1 5) (:shr 255 3))))))   ; expected exit code: 63
