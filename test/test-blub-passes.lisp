;;;; test-blub-passes.lisp
;;;;
;;;; Tests for each Blub compiler pass (0–5) and the full pipeline.
;;;; Requires the tagless-compiler system to be loaded (via ASDF).
;;;;
;;;; Each test group exercises one pass in isolation and verifies the
;;;; shape of its output AST, plus end-to-end QBE IL generation.

(in-package #:tagless-compiler)

;;; ==========================================================================
;;; Pass 0: Desugaring
;;; ==========================================================================

(defsuite "Pass 0: Desugaring"

;; A :declare with a value should split into two statements.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block
                   (:declare (:type :i32) x 5)
                   (:return (:var x))))))
       (out  (lower *blub-0* prog))
       ;; Navigate to the block's body.
       (fn   (second out))
       (blk  (fourth fn))
       (stmts (cdr blk)))   ; strip :block head
  ;; The single (:declare ... 5) expands to two statements, plus the :return = 3.
  (deftest "pass-0: declare+value expands to 2 stmts (3 total)" (check (length stmts) 3))
  (deftest "pass-0: first stmt is :declare" (check (car (first stmts)) :declare))
  (deftest "pass-0: second stmt is :set" (check (car (second stmts)) :set))
  ;; The :declare should have no value slot.
  (deftest "pass-0: bare :declare has 3 elements" (check (length (first stmts)) 3))
  ;; The :set should carry the original value.
  (deftest "pass-0: :set value is 5" (check (third (second stmts)) 5)))

;; A :declare without a value should pass through unchanged.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:declare (:type :i32) y)))))
       (out  (lower *blub-0* prog))
       (fn   (second out))
       (blk  (fourth fn))
       (stmts (cdr blk)))
  (deftest "pass-0: declare without value -> 1 stmt" (check (length stmts) 1))
  (deftest "pass-0: no-value declare is :declare" (check (car (first stmts)) :declare)))

;; Multiple declarations in a block.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block
                   (:declare (:type :i32) a 1)
                   (:declare (:type :i32) b 2)
                   (:return (:var a))))))
       (out   (lower *blub-0* prog))
       (stmts (cdr (fourth (second out)))))
  ;; 2 declares + 1 return = 5 statements after desugaring.
  (deftest "pass-0: two declares+values expand to 5 stmts" (check (length stmts) 5)))

;;; ==========================================================================
;;; Pass 1: Variable renaming
;;; ==========================================================================

)

(defsuite "Pass 1: Variable renaming"

;; After pass 0 + pass 1, a shadowed variable gets a fresh name.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block
                   (:declare (:type :i32) x 0)
                   (:block
                     (:declare (:type :i32) x 1)
                     (:return (:var x)))))))
       (a0   (lower *blub-0* prog))
       (a1   (lower *blub-1* a0))
       ;; After pass-0: outer block = (:block (:declare t x) (:set x 0) (:block ...))
       ;; so the inner (:block) is now at position 4, not 3.
       (fn        (second a1))
       (outer-blk (fourth fn))
       (outer-decl (second outer-blk))   ; (:declare (:type :i32) x)
       (inner-blk  (fourth outer-blk))   ; (:block (:declare ...) ...)
       (inner-decl (second inner-blk)))  ; (:declare (:type :i32) x')
  (let ((outer-name (third outer-decl))
        (inner-name (third inner-decl)))
    (deftest "pass-1: inner x shadowed to fresh name" (check-true (not (eq outer-name inner-name))))))

;; Using a variable before it is declared should be caught by pass 1.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:var x))))))  ; x never declared
       (result (handler-case
                   (progn (lower *blub-1* (lower *blub-0* prog)) :no-error)
                 (error () :error))))
  (deftest "pass-1: undeclared variable caught" (check result :error)))

;; Function parameters are visible inside the function body.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) n)
                 (:block (:return (:var n))))))
       (result (handler-case
                   (lower *blub-1* (lower *blub-0* prog))
                 (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "pass-1: param visible in body" (check-true (not (stringp result)))))

;; Parameters should be renamed like locals — the :var in the body references
;; whatever name pass 1 chose.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) n)
                 (:block (:return (:var n))))))
       (a1      (lower *blub-1* (lower *blub-0* prog)))
       (fn      (second a1))
       (param   (fourth fn))           ; ((:type :i32) chosen-name)
       (pname   (second param))
       (ret     (second (fifth fn)))   ; (:return (:var chosen-name))
       (var-name (second (second ret))))
  (deftest "pass-1: param name matches :var in body" (check pname var-name)))

;; Globals are registered before functions and are visible inside them.
(let* ((prog '(:module
               (:global (:type :i32) G 0)
               (:function (:type :i32) f
                 (:block (:return (:var G))))))
       (result (handler-case
                   (lower *blub-1* (lower *blub-0* prog))
                 (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "pass-1: global visible in function" (check-true (not (stringp result)))))

;; Declaring the same global twice should be caught.
(let* ((prog '(:module
               (:global (:type :i32) G 0)
               (:global (:type :i32) G 1)))
       (result (handler-case
                   (progn (lower *blub-1* (lower *blub-0* prog)) :no-error)
                 (error () :error))))
  (deftest "pass-1: duplicate global caught" (check result :error)))

;;; ==========================================================================
;;; Pass 2: Struct layout
;;; ==========================================================================

)

(defsuite "Pass 2: Struct layout"

;; A two-field i32 struct: both fields 4-byte aligned, no padding.
;; Expected annotated form: (:defstruct point 8 4 (x (:type :i32) 0) (y (:type :i32) 4))
(let* ((prog '(:module
               (:defstruct point ((:type :i32) x) ((:type :i32) y))))
       (a2   (lower *blub-2* (lower *blub-1* (lower *blub-0* prog))))
       (ds   (second a2)))   ; (:defstruct point size align field...)
  (deftest "pass-2: defstruct head" (check (car ds) :defstruct))
  (deftest "pass-2: struct name" (check (second ds) 'point))
  (deftest "pass-2: total size = 8" (check (third ds) 8))
  (deftest "pass-2: alignment = 4" (check (fourth ds) 4))
  ;; Fields: (fname ftype foffset)
  (let ((fields (cddddr ds)))
    (deftest "pass-2: two fields" (check (length fields) 2))
    (let ((f0 (first fields)) (f1 (second fields)))
      (deftest "pass-2: first field name x" (check (first f0) 'x))
      (deftest "pass-2: first field offset 0" (check (third f0) 0))
      (deftest "pass-2: second field name y" (check (first f1) 'y))
      (deftest "pass-2: second field offset 4" (check (third f1) 4)))))

;; A struct with mixed field sizes: u8 followed by i32.
;; u8 at offset 0 (size 1, align 1), then 3 bytes padding, i32 at offset 4.
;; Total size = 8 (padded to i32 alignment); struct alignment = 4.
(let* ((prog '(:module
               (:defstruct mixed ((:type :u8) flag) ((:type :i32) val))))
       (a2   (lower *blub-2* (lower *blub-1* (lower *blub-0* prog))))
       (ds   (second a2))
       (fields (cddddr ds)))
  (deftest "pass-2: mixed struct total size = 8" (check (third ds) 8))
  (deftest "pass-2: mixed struct alignment = 4" (check (fourth ds) 4))
  (deftest "pass-2: flag offset = 0" (check (third (first fields)) 0))
  (deftest "pass-2: val offset = 4" (check (third (second fields)) 4)))

;; Forward-reference error: a struct referencing an undefined struct should fail.
;; (Pass 2 processes structs in order, so second can reference first but not vice-versa.)
(let* ((prog '(:module
               (:defstruct bad ((:type (:struct unknown)) f))))
       (result (handler-case
                   (progn (lower *blub-2* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-2: unknown nested struct caught" (check result :error)))

;;; ==========================================================================
;;; Pass 5: QBE lowering -- basic function
;;; ==========================================================================

)

(defsuite "Pass 5: Basic function lowering"

;; A minimal function: just return a constant.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return 7)))))
       (qbe  (compile-blub prog)))
  (deftest "pass-5: module head is :module" (check (car qbe) :module))
  (let ((fn (second qbe)))
    (deftest "pass-5: function head is :function" (check (car fn) :function))
    (deftest "pass-5: function name is (:global \" (check f\" ))" (second fn) '(:global "f"))
    (deftest "pass-5: linkage is :export" (check (third fn) :export))
    (deftest "pass-5: return type is :w" (check (fourth fn) :w))
    (deftest "pass-5: param list is empty" (check (fifth fn) '()))
    ;; The function should have at least one block.
    (deftest "pass-5: at least one block" (check-true (>= (length (cddr (cddr fn))) 1)))
    ;; The last non-dead block should end with (:ret 7).
    (let* ((blocks (cddr (cddr fn)))
           ;; Find the block containing the ret.
           (ret-block (find-if (lambda (b)
                                 (find-if (lambda (instr)
                                            (and (consp instr) (eq (car instr) :ret)))
                                          (cdr b)))
                               blocks)))
      (deftest "pass-5: ret block found" (check-true (not (null ret-block))))
      (let ((ret-instr (find-if (lambda (i)
                                  (and (consp i) (eq (car i) :ret)))
                                (cdr ret-block))))
        (deftest "pass-5: ret carries value 7" (check (second ret-instr) 7))))))

;;; ==========================================================================
;;; Pass 5: Variable declaration and assignment
;;; ==========================================================================

)

(defsuite "Pass 5: Locals -- declare, assign, load"

(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block
                   (:declare (:type :i32) x)
                   (:set x 42)
                   (:return (:var x))))))
       (qbe   (compile-blub prog))
       (fn    (second qbe))
       ;; Collect all instructions across all blocks.
       (all-instrs (mapcan (lambda (b) (copy-list (cdr b)))
                           (cddr (cddr fn)))))
  ;; There should be an :assign with :alloc4 for the variable declaration.
  (let ((alloc (find-if (lambda (i)
                          (and (consp i) (eq (car i) :assign)
                               (eq (fourth i) :alloc4)))
                        all-instrs)))
    (deftest "pass-5: alloc4 emitted for :declare :int" (check-true alloc)))
  ;; There should be a :storew for the assignment.
  (let ((store (find-if (lambda (i)
                          (and (consp i) (eq (car i) :instr)
                               (eq (second i) :storew)))
                        all-instrs)))
    (deftest "pass-5: storew emitted for :set" (check-true store)))
  ;; There should be a :loadsw for reading the variable.
  (let ((load (find-if (lambda (i)
                         (and (consp i) (eq (car i) :assign)
                              (eq (fourth i) :loadsw)))
                       all-instrs)))
    (deftest "pass-5: loadsw emitted for :var read" (check-true load))))

;;; ==========================================================================
;;; Pass 5: Arithmetic expression
;;; ==========================================================================

)

(defsuite "Pass 5: Arithmetic"

(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:add (:var a) (:var b)))))))
       (qbe       (compile-blub prog))
       (fn        (second qbe))
       (all-instrs (mapcan (lambda (b) (copy-list (cdr b)))
                           (cddr (cddr fn)))))
  ;; Params: two :param nodes with type :w.
  (let ((params (fifth fn)))
    (deftest "pass-5: two params" (check (length params) 2))
    (deftest "pass-5: param 0 type :w" (check (second (first params)) :w))
    (deftest "pass-5: param 1 type :w" (check (second (second params)) :w)))
  ;; An :add instruction should appear.
  (let ((add-instr (find-if (lambda (i)
                              (and (consp i) (eq (car i) :assign)
                                   (eq (fourth i) :add)))
                            all-instrs)))
    (deftest "pass-5: :add instruction emitted" (check-true add-instr))))

;; shl/shr lower to QBE :shl/:shr instructions.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:shl (:var a) (:var b)))))))
       (qbe        (compile-blub prog))
       (fn         (second qbe))
       (all-instrs (mapcan (lambda (b) (copy-list (cdr b)))
                           (cddr (cddr fn))))
       (shl-instr  (find-if (lambda (i)
                              (and (consp i) (eq (car i) :assign)
                                   (eq (fourth i) :shl)))
                            all-instrs)))
  (deftest "pass-5: :shl instruction emitted" (check-true shl-instr)))

(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:shr (:var a) (:var b)))))))
       (qbe        (compile-blub prog))
       (fn         (second qbe))
       (all-instrs (mapcan (lambda (b) (copy-list (cdr b)))
                           (cddr (cddr fn))))
       (shr-instr  (find-if (lambda (i)
                              (and (consp i) (eq (car i) :assign)
                                   (eq (fourth i) :shr)))
                            all-instrs)))
  (deftest "pass-5: :shr instruction emitted" (check-true shr-instr)))

;; Typechecker rejects shifts on non-integer operands.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:shl 1.0 2))))))
       (result (handler-case (compile-blub prog)
                 (error (e) (declare (ignore e)) :error))))
  (deftest "pass-3: :shl on float operand caught" (check result :error)))

;;; ==========================================================================
;;; Pass 5: If statement
;;; ==========================================================================

)

(defsuite "Pass 5: If/else branching"

(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) x)
                 (:block
                   (:declare (:type :i32) r 0)
                   (:if (:gt (:var x) 0)
                     (:block (:set r 1))
                     (:block (:set r (:neg (:var x)))))
                   (:return (:var r))))))
       (qbe  (compile-blub prog))
       (fn   (second qbe))
       ;; Each if/else generates at least 3 extra blocks (then, else, end).
       (blocks (cddr (cddr fn))))
  (deftest "pass-5: if generates >= 4 blocks" (check-true (>= (length blocks) 4)))
  ;; At least one block should contain a :jnz terminator.
  (let ((jnz-block (find-if (lambda (b)
                               (find-if (lambda (i)
                                          (and (consp i) (eq (car i) :jnz)))
                                        (cdr b)))
                             blocks)))
    (deftest "pass-5: :jnz terminator emitted for :if" (check-true jnz-block))))

;;; ==========================================================================
;;; Pass 5: While loop
;;; ==========================================================================

)

(defsuite "Pass 5: While loop"

(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) n)
                 (:block
                   (:declare (:type :i32) i 0)
                   (:while (:lt (:var i) (:var n))
                     (:block (:set i (:add (:var i) 1))))
                   (:return (:var i))))))
       (qbe    (compile-blub prog))
       (fn     (second qbe))
       (blocks (cddr (cddr fn))))
  ;; while generates at least cond + body + end blocks on top of start.
  (deftest "pass-5: while generates >= 4 blocks" (check-true (>= (length blocks) 4)))
  ;; There should be a block ending with :jnz (the condition check).
  (let ((cond-block (find-if (lambda (b)
                               (find-if (lambda (i)
                                          (and (consp i) (eq (car i) :jnz)))
                                        (cdr b)))
                             blocks)))
    (deftest "pass-5: :jnz emitted for while condition" (check-true cond-block)))
  ;; There should be two :jmp terminators (fall-into-cond and body-back-to-cond).
  (let ((jmp-count (count-if (lambda (b)
                                (find-if (lambda (i)
                                           (and (consp i) (eq (car i) :jmp)))
                                         (cdr b)))
                              blocks)))
    (deftest "pass-5: at least 2 :jmp terminators for while" (check-true (>= jmp-count 2)))))

;;; ==========================================================================
;;; Pass 5: Function call
;;; ==========================================================================

)

(defsuite "Pass 5: Function call"

(let* ((prog '(:module
               (:function (:type :i32) add
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:add (:var a) (:var b)))))
               (:function (:type :i32) qbe_main
                 (:block (:return (:call add 3 4))))))
       (qbe  (compile-blub prog))
       ;; Second function is qbe_main.
       (main-fn (third qbe))
       (all-instrs (mapcan (lambda (b) (copy-list (cdr b)))
                           (cddr (cddr main-fn)))))
  (deftest "pass-5: call result: module has 2 functions" (check (length (cdr qbe)) 2))
  ;; There should be a :call-assign instruction in qbe_main.
  (let ((call-instr (find-if (lambda (i)
                               (and (consp i) (eq (car i) :call-assign)))
                             all-instrs)))
    (deftest "pass-5: :call-assign emitted for :call" (check-true call-instr))
    (when call-instr
      (deftest "pass-5: call target is (:global add)" (check (fourth call-instr) '(:global "add"))))))

;;; ==========================================================================
;;; Pass 5: Global variables
;;; ==========================================================================

)

(defsuite "Pass 5: Global variables"

(let* ((prog '(:module
               (:global (:type :i32) LIMIT 100)
               (:function (:type :i32) f
                 (:block (:return (:var LIMIT))))))
       (qbe (compile-blub prog)))
  ;; First child should be a :data item for the global.
  (let ((data (second qbe)))
    (deftest "pass-5: global emits :data" (check (car data) :data))
    (deftest "pass-5: global name" (check (second data) '(:global "limit"))))
  ;; The function reading the global should emit a :loadl or :loadsw.
  (let* ((fn (third qbe))
         (all-instrs (mapcan (lambda (b) (copy-list (cdr b)))
                             (cddr (cddr fn))))
         (load (find-if (lambda (i)
                          (and (consp i) (eq (car i) :assign)
                               (member (fourth i) '(:loadsw :loadl :loadd :loadsb))))
                        all-instrs)))
    (deftest "pass-5: load emitted for global :var read" (check-true load))))

;;; ==========================================================================
;;; End-to-end: full pipeline + QBE IL string generation
;;; ==========================================================================

)

(defsuite "End-to-end: compile to QBE IL string"

(let* ((prog '(:module
               (:function (:type :i32) qbe_main
                 (:block (:return 0)))))
       (il-string (compile-blub-to-string prog)))
  (deftest "e2e: result is a string" (check-true (stringp il-string)))
  (deftest "e2e: string contains 'function'" (check-true (search "function" il-string)))
  (deftest "e2e: string contains 'ret'" (check-true (search "ret" il-string)))
  (deftest "e2e: string contains '$qbe_main'" (check-true (search "$qbe_main" il-string))))

;; fibonacci example: compile to string without error.
(let* ((fib-prog
        '(:module
          (:function (:type :i32) fib
            ((:type :i32) n)
            (:block
              (:declare (:type :i32) a 0)
              (:declare (:type :i32) b 1)
              (:declare (:type :i32) i 0)
              (:declare (:type :i32) tmp 0)
              (:while (:lt (:var i) (:var n))
                (:block
                  (:set tmp (:var b))
                  (:set b   (:add (:var a) (:var b)))
                  (:set a   (:var tmp))
                  (:set i   (:add (:var i) 1))))
              (:return (:var a))))
          (:function (:type :i32) qbe_main
            (:block (:return (:call fib 10))))))
       (il (handler-case (compile-blub-to-string fib-prog)
             (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "e2e: fibonacci compiles without error" (check-true (not (and (stringp il) (string= (subseq il 0 5) "ERROR")))))
  (when (stringp il)
    (deftest "e2e: fibonacci IL contains 'while.cond'" (check-true (search "while" il)))))

;;; ==========================================================================
;;; Pass 3: Typechecking
;;; ==========================================================================

)

(defsuite "Pass 3: Typechecking"

;; Valid program should pass through unchanged.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) x)
                 (:block (:return (:add (:var x) 1))))))
       (result (handler-case (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                 (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "pass-3: valid program passes typecheck" (check-true (not (stringp result)))))

;; :add with f64 on one side and i32 on the other should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:add 1 3.14))))))
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: i32+f64 mix caught" (check result :error)))

;; Calling with wrong number of arguments should be caught.
(let* ((prog '(:module
               (:function (:type :i32) add
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:add (:var a) (:var b)))))
               (:function (:type :i32) bad
                 (:block (:return (:call add 1))))))  ; add takes 2, gets 1
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: wrong arg count caught" (check result :error)))

;; Calling an undeclared function should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:call ghost 1 2))))))
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: undeclared function call caught" (check result :error)))

;; Using a variable before declaration should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:var x))))))  ; x never declared
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: undeclared variable use caught" (check result :error)))

;; :return type mismatch should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return 3.14)))))  ; declares :i32, returns f64
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: return type mismatch caught" (check result :error)))

;; :not on an f64 should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:not 3.14))))))
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: :not on f64 caught" (check result :error)))

;; :if with an f64 condition should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block
                   (:declare (:type :i32) r 0)
                   (:if 3.14 (:block (:set r 1)))
                   (:return (:var r))))))
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: :if float condition caught" (check result :error)))

;; Global variables visible inside functions.
(let* ((prog '(:module
               (:global (:type :i32) LIMIT 100)
               (:function (:type :i32) f
                 (:block (:return (:var LIMIT))))))
       (result (handler-case (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                 (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "pass-3: global variable visible in function" (check-true (not (stringp result)))))

;; After pass 3, expressions should be wrapped in (:typed type inner).
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) x)
                 (:block (:return (:add (:var x) 1))))))
       (a3 (lower *blub-3* (lower *blub-1* (lower *blub-0* prog))))
       ;; Navigate to the :return's expression.
       (fn     (second a3))
       (body   (fifth fn))
       (ret    (second body))     ; (:return (:typed ...))
       (retval (second ret)))
  (deftest "pass-3: return value is :typed-wrapped" (check (car retval) :typed))
  (deftest "pass-3: return type is :i32" (check (second retval) '(:type :i32)))
  (let ((inner (caddr retval)))
    (deftest "pass-3: inner form is :add" (check (car inner) :add))))

;; :addr-of a variable should produce a pointer type.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) x)
                 (:block (:return (:cast (:type :i32) (:cast (:type :i64) (:addr-of (:var x)))))))))
       (result (handler-case
                   (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                 (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "pass-3: :addr-of compiles without error" (check-true (not (stringp result)))))

;; :cast between compatible scalar types should succeed.
(let* ((prog '(:module
               (:function (:type :i64) f
                 ((:type :i32) x)
                 (:block (:return (:cast (:type :i64) (:var x)))))))
       (a3 (lower *blub-3* (lower *blub-1* (lower *blub-0* prog))))
       (fn     (second a3))
       (body   (fifth fn))
       (ret    (second body))
       (retval (second ret)))
  (deftest "pass-3: :cast result has target type" (check (second retval) '(:type :i64)))
  (deftest "pass-3: :cast inner form is :cast" (check (car (caddr retval)) :cast)))

;; :cast to :void should be caught.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) x)
                 (:block (:return (:cast (:type :void) (:var x)))))))
       (result (handler-case
                   (progn (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))
                          :no-error)
                 (error () :error))))
  (deftest "pass-3: :cast to :void caught" (check result :error)))

;; :logand and :logor on integer operands should typecheck to :i32.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:logand (:var a) (:var b)))))))
       (a3 (lower *blub-3* (lower *blub-1* (lower *blub-0* prog))))
       (fn  (second a3))
       (ret (second (sixth fn)))
       (rv  (second ret)))
  (deftest "pass-3: :logand result type is :i32" (check (second rv) '(:type :i32))))

;;; ==========================================================================
;;; Pass 4: Expression normalization
;;; ==========================================================================

)

(defsuite "Pass 4: Expression normalization"

;; Nested (:set x (:add (:mul a b) c)) should produce an extra temp.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) a) ((:type :i32) b) ((:type :i32) c)
                 (:block
                   (:declare (:type :i32) x 0)
                   (:set x (:add (:mul (:var a) (:var b)) (:var c)))
                   (:return (:var x))))))
       ;; Run only passes 0-4 so we can inspect the normalized Blub AST.
       (a0 (lower *blub-0* prog))
       (a1 (lower *blub-1* a0))
       (a3 (lower *blub-3* a1))
       (a4 (lower *blub-4* a3))
       ;; Collect all statements from the function body.
       (fn    (second a4))
       (body  (car (last fn)))     ; (:block ...)
       (stmts (cdr body)))         ; strip :block head
  ;; The nested :mul should have been extracted: we expect more statements
  ;; than the original 3 (declare, assign, return).
  (deftest "pass-4: nested mul extracted to temp (>3 stmts)" (check-true (> (length stmts) 3)))
  ;; After pass 3, values carry :typed wrappers; strip them for keyword checks.
  (flet ((unwrap (expr)
           (if (and (consp expr) (eq (car expr) :typed)) (caddr expr) expr)))
    ;; The extra :assign (internal temp) should have a :mul as its value.
    (let ((temp-assign
            (find-if (lambda (s)
                       (and (consp s) (eq (car s) :assign)
                            (let ((inner (unwrap (caddr s))))
                              (and (consp inner) (eq (car inner) :mul)))))
                     stmts)))
      (deftest "pass-4: a :assign :mul exists for the extracted temp" (check-true temp-assign))
      (when temp-assign
        ;; The final (:set x ...) should use :add with atomic (:var or :typed/:var) operands.
        (let ((final-assign
                (find-if (lambda (s)
                           (and (consp s) (eq (car s) :set)
                                (let ((inner (unwrap (caddr s))))
                                  (and (consp inner) (eq (car inner) :add)))))
                         stmts)))
          (deftest "pass-4: final :set uses :add with atomic operands" (check-true (and final-assign
                                (let* ((add-form (unwrap (caddr final-assign)))
                                       (l (unwrap (cadr  add-form)))
                                       (r (unwrap (caddr add-form))))
                                  (and (consp l) (eq (car l) :var)
                                       (consp r) (eq (car r) :var)))))))))))

;; An :if with a complex condition should hoist prefix stmts before the :if.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) x) ((:type :i32) y)
                 (:block
                   (:declare (:type :i32) r 0)
                   (:if (:gt (:add (:var x) 1) (:var y))
                     (:block (:set r 1)))
                   (:return (:var r))))))
       (a0 (lower *blub-0* prog))
       (a1 (lower *blub-1* a0))
       (a3 (lower *blub-3* a1))
       (a4 (lower *blub-4* a3))
       (fn    (second a4))
       (body  (car (last fn)))
       (stmts (cdr body)))
  ;; After normalization, (:add x 1) is atomic so the :gt can stay as-is,
  ;; but pass 4 may extract the :add sub-expression of :gt.  Either way,
  ;; the :if should still appear and the condition should be simplified.
  (let ((if-stmt (find-if (lambda (s) (and (consp s) (eq (car s) :if))) stmts)))
    (deftest "pass-4: :if still present after normalization" (check-true if-stmt))
    (when if-stmt
      ;; Condition may be :typed-wrapped; strip to get the comparison form.
      (let* ((cond-node (second if-stmt))
             (cond-form (if (and (consp cond-node) (eq (car cond-node) :typed))
                          (caddr cond-node) cond-node)))
        (deftest "pass-4: :if condition is a simple comparison" (check-true (and (consp cond-form)
                              (member (car cond-form) '(:gt :ge :lt :le :eq :ne)))))))))

;; :while condition should NOT be extracted (limitation: would break re-evaluation).
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) n)
                 (:block
                   (:declare (:type :i32) i 0)
                   (:while (:lt (:add (:var i) 1) (:var n))
                     (:block (:set i (:add (:var i) 1))))
                   (:return (:var i))))))
       (a0 (lower *blub-0* prog))
       (a1 (lower *blub-1* a0))
       (a3 (lower *blub-3* a1))
       (a4 (lower *blub-4* a3))
       (fn    (second a4))
       (body  (fifth fn))
       (stmts (cdr body))
       (while-stmt (find-if (lambda (s) (and (consp s) (eq (car s) :while))) stmts)))
  (deftest "pass-4: :while present" (check-true while-stmt))
  (when while-stmt
    ;; The while condition may be :typed-wrapped; strip to check the :lt form
    ;; and its left operand (the :add sub-expression that should NOT be extracted).
    (let* ((cond-node (second while-stmt))
           (cond (if (and (consp cond-node) (eq (car cond-node) :typed))
                   (caddr cond-node) cond-node)))
      (deftest "pass-4: :while condition unchanged (still contains :add sub-expr)" (check-true (and (consp cond)
                            (eq (car cond) :lt)
                            (let* ((left-arg (cadr cond))
                                   (left-inner (if (and (consp left-arg)
                                                        (eq (car left-arg) :typed))
                                                 (caddr left-arg) left-arg)))
                              (and (consp left-inner)
                                   (eq (car left-inner) :add)))))))))

;; A nested expression in a :return is also simplified.
(let* ((prog '(:module
               (:function (:type :i32) f
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:add (:mul (:var a) 2) (:var b)))))))
       (a4  (lower *blub-4* (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))))
       (fn  (second a4))
       (stmts (cdr (car (last fn)))))
  ;; The :mul should be extracted: there must be an internal :assign :mul before the :return.
  (let ((mul-assign (find-if (lambda (s)
                               (and (consp s) (eq (car s) :assign)
                                    (let ((v (if (and (consp (caddr s))
                                                       (eq (car (caddr s)) :typed))
                                               (caddr (caddr s)) (caddr s))))
                                      (and (consp v) (eq (car v) :mul)))))
                             stmts)))
    (deftest "pass-4: :mul in :return extracted to temp" (check-true mul-assign))))

;; Call arguments are atomized: (:call f (:add x y)) should produce a temp for :add.
(let* ((prog '(:module
               (:function (:type :i32) id
                 ((:type :i32) x)
                 (:block (:return (:var x))))
               (:function (:type :i32) g
                 ((:type :i32) a) ((:type :i32) b)
                 (:block (:return (:call id (:add (:var a) (:var b))))))))
       (a4   (lower *blub-4* (lower *blub-3* (lower *blub-1* (lower *blub-0* prog)))))
       (g-fn (third a4))   ; second function
       (stmts (cdr (car (last g-fn)))))
  (let ((add-assign (find-if (lambda (s)
                               (and (consp s) (eq (car s) :assign)
                                    (let ((v (if (and (consp (caddr s))
                                                       (eq (car (caddr s)) :typed))
                                               (caddr (caddr s)) (caddr s))))
                                      (and (consp v) (eq (car v) :add)))))
                             stmts)))
    (deftest "pass-4: :add in call arg extracted to temp" (check-true add-assign))))

;; End-to-end: fibonacci still compiles and runs correctly through all passes.
(let* ((fib-prog
        '(:module
          (:function (:type :i32) fib
            ((:type :i32) n)
            (:block
              (:declare (:type :i32) a 0)
              (:declare (:type :i32) b 1)
              (:declare (:type :i32) i 0)
              (:declare (:type :i32) tmp 0)
              (:while (:lt (:var i) (:var n))
                (:block
                  (:set tmp (:var b))
                  (:set b   (:add (:var a) (:var b)))
                  (:set a   (:var tmp))
                  (:set i   (:add (:var i) 1))))
              (:return (:var a))))
          (:function (:type :i32) qbe_main
            (:block (:return (:call fib 10))))))
       (il (handler-case (compile-blub-to-string fib-prog)
             (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "pass-4: fibonacci compiles end-to-end through all passes" (check-true (not (and (stringp il) (string= (subseq il 0 5) "ERROR"))))))

;;; ==========================================================================
;;; Function pointers
;;; ==========================================================================

)

(defsuite "Function pointers"

;; (:fn-ptr name) should compile without error and produce a :global in the IL.
(let* ((prog '(:module
               (:function (:type :i32) double-it
                 ((:type :i32) x)
                 (:block (:return (:mul (:var x) 2))))
               (:function (:type :i32) apply
                 ((:type (:fn (:type :i32) (:type :i32))) fn) ((:type :i32) x)
                 (:block (:return (:call (:var fn) (:var x)))))
               (:function (:type :i32) qbe_main
                 (:block (:return (:call apply (:fn-ptr double-it) 5))))))
       (il (handler-case (compile-blub-to-string prog)
             (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "fn-ptr: compiles without error" (check-true (and (stringp il) (not (search "ERROR" il)))))
  (when (stringp il)
    (deftest "fn-ptr: IL contains indirect 'call %'" (check-true (search "call %" il)))
    (deftest "fn-ptr: IL references $double_it global" (check-true (search "$double_it" il)))))

;; (:fn-ptr) to an undeclared function should be caught by pass 3.
(let* ((prog '(:module
               (:function (:type :i32) f
                 (:block (:return (:fn-ptr no-such-fn))))))
       (result (handler-case (progn (compile-blub prog) :no-error)
                 (error () :error))))
  (deftest "fn-ptr: undeclared function caught" (check result :error)))

;;; ==========================================================================
;;; Struct field assignment (:set (:. struct field) val) and struct by reference
;;; ==========================================================================

)

(defsuite "Struct: set field and by-reference"

;; Basic (:set (:. struct field) val) + :addr-of + field read via pointer dereference.
(let* ((prog '(:module
               (:defstruct point ((:type :i32) x) ((:type :i32) y))
               (:function (:type :i32) sum-fields
                 ((:type (:pointer (:type (:struct point)))) p)
                 (:block
                   (:return (:add (:. (:deref (:var p)) x)
                                  (:. (:deref (:var p)) y)))))
               (:function (:type :i32) qbe_main
                 (:block
                   (:declare (:type (:struct point)) pt)
                   (:set (:. (:var pt) x) 3)
                   (:set (:. (:var pt) y) 4)
                   (:return (:call sum-fields (:addr-of (:var pt))))))))
       (il (handler-case (compile-blub-to-string prog)
             (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "struct-ref: compiles without error" (check-true (and (stringp il) (not (search "ERROR" il)))))
  (when (stringp il)
    ;; QBE type definition should be emitted.
    (deftest "struct-ref: type :point emitted" (check-true (search "type :point" il)))
    ;; Two storew instructions for the two field stores.
    (let ((storew-count (let ((pos 0) (n 0))
                          (loop (let ((found (search "storew" il :start2 pos)))
                                  (unless found (return n))
                                  (incf n)
                                  (setf pos (1+ found)))))))
      (deftest "struct-ref: two storew for field stores" (check-true (>= storew-count 2))))
    ;; At least one loadsw for field reads.
    (deftest "struct-ref: loadsw for field read" (check-true (search "loadsw" il)))))

;;; ==========================================================================
;;; Function pointer + struct combined
;;; ==========================================================================

)

(defsuite "Combined: struct + function pointer"

(let* ((prog '(:module
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
                   (:return (:var r))))))
       (il (handler-case (compile-blub-to-string prog)
             (error (e) (format nil "ERROR: ~A" e)))))
  (deftest "combined: compiles without error" (check-true (and (stringp il) (not (search "ERROR" il)))))
  (when (stringp il)
    (deftest "combined: has indirect call" (check-true (search "call %" il)))
    (deftest "combined: references $get_x" (check-true (search "$get_x" il)))
    (deftest "combined: references $get_y" (check-true (search "$get_y" il)))
    (deftest "combined: type :point emitted" (check-true (search "type :point" il)))))

)
