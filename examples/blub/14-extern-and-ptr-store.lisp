;; Exercises :extern (calling a C-runtime function) plus (:set (:deref ptr) val)
;; for writing through a pointer. The C driver supplies _ext_alloc which returns
;; a heap pointer; blub writes a u64 through it, reads it back, and returns it.
;;
;;; #include <stdlib.h>
;;; #include <stdint.h>
;;; extern int qbe_main(void);
;;; uint64_t _ext_alloc(uint64_t n) { return (uint64_t)malloc(n); }
;;; int main(void) { return qbe_main(); }
(:module
  (:extern (:type :u64) _ext_alloc ((:type :u64) n))

  (:function (:type :i32) qbe_main
    (:block
      (:declare (:type (:pointer (:type :u64))) p
                (:cast (:type (:pointer (:type :u64))) (:call _ext_alloc 8)))
      (:set (:deref (:var p)) 99)
      (:return (:cast (:type :i32) (:deref (:var p))))))) ; expected exit code: 99
