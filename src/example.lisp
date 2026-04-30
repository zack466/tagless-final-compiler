(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

;; --- Interpreter 1: Arithmetic Evaluator ---
(defvar arith-eval (make-interpreter))

(def-op arith-eval (:add a b)
  (+ (recurse a) (recurse b)))

(def-op arith-eval (:mul a b)
  (* (recurse a) (recurse b)))

(def-op arith-eval (:inc a)
  (1+ (recurse a)))

;; --- Interpreter 2: String Representation ---
(defvar string-repr (make-interpreter))

(def-op string-repr (:add a b)
  (format nil "(Add ~A ~A)" (recurse a) (recurse b)))

(def-op string-repr (:mul a b)
  (format nil "(Mul ~A ~A)" (recurse a) (recurse b)))

(def-op string-repr (:inc a)
  (format nil "(Inc ~A)" (recurse a)))

;; --- Interpreter 3: Partial pass (recurses through unknown ops) ---
(defvar partial-rewrite (make-interpreter :on-unknown :recurse))

(def-op partial-rewrite (:square a)
  (list :mul (recurse a) (recurse a)))

;; --- Interpreter 4: Nested-pattern demo ---
;;
;; A toy "let" form: (:let ((var val) ...) body) -- destructures each
;; binding into (var val) and rewrites to a chain of additions for demo
;; purposes (real semantics would need an environment).
(defvar nested-demo (make-interpreter))

(def-op nested-demo (:pair (a b) c)
  ;; First arg is destructured into a, b; second is c.
  (list :triple a b c))

(def-op nested-demo (:opt-pair (a &optional (b 99)) c)
  (list :triple-with-default a b c))

(def-op nested-demo (:rest-pair &rest (x y &rest tail))
  (list :head-pair x y :tail tail))

;; --- Interpreter 5: Statement splicing demo ---
;;
;; A C-like lowering pass: (:set (int x) v) is sugar for "declare a variable
;; and assign", and expands to two statements. The expansion is only legal
;; in a statement context -- the caller decides by using RECURSE-SPLICE
;; (statement context) vs RECURSE (expression context).

(defvar c-lower (make-interpreter :on-unknown :passthrough))

(def-op c-lower (:block &rest stmts)
  ;; Statement context: each child is lowered with RECURSE-SPLICE, and
  ;; MAPCAN flattens the resulting per-child lists into one body.
  (cons :block (mapcan #'recurse-splice stmts)))

(def-op c-lower (:if cond then else)
  ;; cond is an expression slot; then/else are single-statement slots.
  ;; Using RECURSE here means a splice in any of these positions errors.
  (list :if (recurse cond) (recurse then) (recurse else)))

(def-op c-lower (:set (type var) val)
  ;; This is the splicing rule. Destructures (type var) using a nested
  ;; pattern and returns two statements.
  (splice (list :var type var)
          (list :set var (recurse val))))

;; The :swap rule introduces a binding (a temporary holding A's old value)
;; and then assigns A and B. A literal name like TMP would capture the
;; user's variable if they wrote (:swap tmp other) -- FRESH-NAME mints a
;; name that can't collide with anything the user wrote.
(def-op c-lower (:swap a b)
  (let ((tmp (fresh-name "tmp")))
    (splice (list :var :auto tmp)
            (list :set tmp a)
            (list :set a (recurse b))
            (list :set b tmp))))

;; --- Evaluation ---

(defvar *program* '(:mul (:add 1 2) (:inc 5)))

(lower arith-eval *program*)
(lower string-repr *program*)
(lower partial-rewrite '(:if (:square 2) (:square 3) (:add (:square 4) 1)))

(lower nested-demo '(:pair (10 20) 30))
(lower nested-demo '(:opt-pair (10) 30))
(lower nested-demo '(:opt-pair (10 20) 30))
(lower nested-demo '(:rest-pair 1 2 3 4 5))

(lower c-lower '(:block (:swap a b)))

