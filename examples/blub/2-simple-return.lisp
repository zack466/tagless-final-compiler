;; Minimal blub program: a function that returns a constant integer.
;; Exercises pass 0 (no desugaring needed), pass 1 (no shadowing),
;; and pass 5 (function + return lowering).
;;
;; Equivalent C:
;;   int qbe_main() { return 42; }
;;
;;; extern int qbe_main();
;;; int main() { return qbe_main(); }
(:module
  (:function (:type :i32) qbe_main
    (:block
      (:return 42))))   ; expected exit code: 42
