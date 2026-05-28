;; Tests global variables, if/else, and boolean expressions.
;;
;; Equivalent C:
;;   int THRESHOLD = 100;
;;   int clamp(int x) {
;;     int result = x;
;;     if (x > THRESHOLD) {
;;       result = THRESHOLD;
;;     }
;;     return result;
;;   }
;;   int qbe_main() { return clamp(150); }
;;
;;; #include <assert.h>
;;; extern int clamp(int x);
;;; extern int qbe_main();
;;; int main() {
;;;     assert(clamp(50)  == 50);
;;;     assert(clamp(100) == 100);
;;;     assert(clamp(150) == 100);
;;;     assert(clamp(0)   == 0);
;;;     return qbe_main();
;;; }
(:module
  (:global (:type :i32) THRESHOLD 100)

  (:function (:type :i32) clamp
    ((:type :i32) x)
    (:block
      (:declare (:type :i32) result 0)
      (:set result (:var x))
      (:if (:gt (:var x) (:var THRESHOLD))
        (:block
          (:set result (:var THRESHOLD))))
      (:return (:var result))))

  (:function (:type :i32) qbe_main
    (:block
      (:return (:call clamp 150)))))   ; expected exit code: 100
