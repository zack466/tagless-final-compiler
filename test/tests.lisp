;;;; tests.lisp -- Tests for the tagless-compiler interpreter framework.
;;;;
;;;; Assumes the TAGLESS-COMPILER package and its interpreter.lisp have
;;;; already been loaded (e.g. via ASDF). Loading this file runs the suite
;;;; and prints a pass/fail summary. With ASDF, you can wire it up by adding
;;;; a :test-op component or simply :load this file after the system loads.

(in-package #:tagless-compiler)

;;; --- Test harness -----------------------------------------------------

(defvar *results* '())

(defmacro check (label form expected &key (test '#'equal))
  "Evaluate FORM and compare its result to EXPECTED via TEST. Print PASS/FAIL
   and record the outcome in *RESULTS*."
  `(let ((actual ,form))
     (if (funcall ,test actual ,expected)
         (progn (format t "  PASS  ~A~%" ,label)
                (push (list :pass ,label) *results*))
         (progn (format t "  FAIL  ~A~%    expected: ~S~%    actual:   ~S~%"
                        ,label ,expected actual)
                (push (list :fail ,label ,expected actual) *results*)))))

(defmacro check-malformed (label form)
  "FORM should signal MALFORMED-OPERATOR-ARGS."
  `(check ,label
          (handler-case (progn ,form :no-error)
            (malformed-operator-args () :malformed))
          :malformed))

;;; --- Original behavior ------------------------------------------------

(format t "~%=== Original behavior ===~%")

(check "arith-eval on *program*"
       (lower arith-eval *program*)
       18)

(check "string-repr on *program*"
       (lower string-repr *program*)
       "(Mul (Add 1 2) (Inc 5))")

(check "partial-rewrite recurses into unknown heads"
       (lower partial-rewrite '(:if (:square 2) (:square 3) (:add (:square 4) 1)))
       '(:if (:mul 2 2) (:mul 3 3) (:add (:mul 4 4) 1)))

;;; --- Lambda-list features ---------------------------------------------

(format t "~%=== def-op lambda-list features ===~%")

;; &rest support
(defvar rest-interp (make-interpreter))
(def-op rest-interp (:sum &rest xs)
  (reduce #'+ (mapcar #'recurse xs)))

(check ":sum with &rest, 0 args"  (lower rest-interp '(:sum)) 0)
(check ":sum with &rest, 1 arg"   (lower rest-interp '(:sum 5)) 5)
(check ":sum with &rest, 4 args"  (lower rest-interp '(:sum 1 2 3 4)) 10)

;; &key support (with defaults)
(defvar key-interp (make-interpreter))
(def-op key-interp (:scale x &key (by 1) (offset 0))
  (+ (* (recurse x) by) offset))

(check ":scale with defaults"     (lower key-interp '(:scale 10)) 10)
(check ":scale with :by"          (lower key-interp '(:scale 10 :by 3)) 30)
(check ":scale with :by :offset"  (lower key-interp '(:scale 10 :by 3 :offset 5)) 35)

;; &optional support (with defaults)
(defvar opt-interp (make-interpreter))
(def-op opt-interp (:maybe-inc x &optional (n 1))
  (+ (recurse x) n))

(check ":maybe-inc default"  (lower opt-interp '(:maybe-inc 5)) 6)
(check ":maybe-inc explicit" (lower opt-interp '(:maybe-inc 5 10)) 15)

;; Mixed: required + &rest
(defvar mixed-interp (make-interpreter))
(def-op mixed-interp (:cons-up head &rest tail)
  (cons (recurse head) (mapcar #'recurse tail)))

(check ":cons-up mixed"
       (lower mixed-interp '(:cons-up 1 2 3 4))
       '(1 2 3 4))

;;; --- Structured malformed-operator-args condition ---------------------

(format t "~%=== Structured malformed-operator-args condition ===~%")

(handler-case
    (progn (lower arith-eval '(:add 1))
           (format t "  FAIL  malformed :add did not error~%")
           (push '(:fail :no-error) *results*))
  (malformed-operator-args (c)
    (format t "  PASS  malformed :add signals malformed-operator-args~%")
    (push '(:pass :error-type) *results*)
    (check "  operator slot"   (malformed-operator-args-operator c)   :add)
    (check "  expression slot" (malformed-operator-args-expression c) '(:add 1))
    (check "  pattern slot"    (malformed-operator-args-pattern c)    '(:add a b))))

(check "outer handler can recover"
       (handler-case (lower arith-eval '(:add 1))
         (malformed-operator-args (c)
           (declare (ignore c))
           :recovered))
       :recovered)

;; Errors signalled from inside the handler body must NOT be reclassified
;; as malformed-operator-args -- they are real downstream errors.
(defvar body-err-interp (make-interpreter))
(def-op body-err-interp (:divide a b)
  (/ (recurse a) (recurse b)))

(check "body errors propagate as themselves (not malformed-operator-args)"
       (handler-case (lower body-err-interp '(:divide 1 0))
         (malformed-operator-args () :wrongly-caught)
         (division-by-zero ()        :correctly-propagated)
         (arithmetic-error ()        :correctly-propagated))
       :correctly-propagated)

(check-malformed "too few positional args" (lower arith-eval '(:add 1)))
(check-malformed "too many positional args" (lower arith-eval '(:add 1 2 3)))
(check-malformed "odd-length keyword block"
                 (lower key-interp '(:scale 10 :by)))
(check-malformed "unknown keyword in &key block"
                 (lower key-interp '(:scale 10 :nope 5)))
(check-malformed "trailing junk with no &rest"
                 (lower key-interp '(:scale 10 :by 3 99)))

;; Parser-time errors: bad lambda-list shape detected at macroexpansion.
(check "parse error on &rest without var"
       (handler-case
           (progn (macroexpand-1 '(def-op key-interp (:bad &rest))) :no-error)
         (error () :parse-error))
       :parse-error)

(check "parse error on &optional after &key"
       (handler-case
           (progn (macroexpand-1 '(def-op key-interp (:bad &key x &optional y))) :no-error)
         (error () :parse-error))
       :parse-error)

;;; --- CL-package symbols as parameter names ----------------------------

(format t "~%=== CL-package symbols as parameter names ===~%")

;; Parameters whose names happen to be CL symbols (TYPE, COUNT, LIST, ...)
;; must work without tripping package locks.
(defvar cl-name-interp (make-interpreter))

(def-op cl-name-interp (:declare-var name type)
  (format nil "var ~A : ~A" (recurse name) (recurse type)))

(check "cl:type as parameter name"
       (lower cl-name-interp '(:declare-var :x :int))
       "var X : INT")

(def-op cl-name-interp (:counted &optional (count 0))
  (recurse count))

(check "cl:count as &optional name (default)"
       (lower cl-name-interp '(:counted))
       0)
(check "cl:count as &optional name (provided)"
       (lower cl-name-interp '(:counted 7))
       7)

(def-op cl-name-interp (:listing &rest list)
  (length list))

(check "cl:list as &rest name"
       (lower cl-name-interp '(:listing 1 2 3 4))
       4)

;;; --- Cross-interpreter dispatch ---------------------------------------

(format t "~%=== Cross-interpreter dispatch ===~%")

;; A rule defined on one interpreter can dispatch to another by calling the
;; generic LOWER explicitly. RECURSE stays bound to self-recursion.
(defvar fold-interp (make-interpreter :on-unknown :recurse))
(def-op fold-interp (:add a b)
  (let ((aa (recurse a)) (bb (recurse b)))
    (if (and (numberp aa) (numberp bb)) (+ aa bb) (list :add aa bb))))

(defvar print-interp (make-interpreter))
(def-op print-interp (:add a b)
  (format nil "(~A + ~A)" (recurse a) (recurse b)))
(def-op print-interp (:embed-folded x)
  (format nil "[folded: ~A]" (lower fold-interp x)))

(check "self-recurse via RECURSE"
       (lower print-interp '(:add 1 (:add 2 3)))
       "(1 + (2 + 3))")

(check "cross-dispatch via generic LOWER"
       (lower print-interp '(:add 1 (:embed-folded (:add 2 (:add 3 4)))))
       "(1 + [folded: 9])")

;;; --- Nested patterns --------------------------------------------------
;;;
;;; def-op lambda lists support nested patterns: any positional, &optional,
;;; or &rest position can itself be a sub-lambda-list with its own
;;; &optional / &rest. (&key vars stay restricted to plain symbols, since
;;; the keyword name is derived from the symbol.)

(format t "~%=== Nested patterns ===~%")

(defvar nested-interp (make-interpreter))

;; Plain nested positional: first arg destructures into a, b.
(def-op nested-interp (:pair (a b) c)
  (list a b c))

(check "nested positional list"
       (lower nested-interp '(:pair (10 20) 30))
       '(10 20 30))

;; Doubly nested: ((a b) c) d.
(def-op nested-interp (:deep ((a b) c) d)
  (list a b c d))

(check "doubly nested positional"
       (lower nested-interp '(:deep ((1 2) 3) 4))
       '(1 2 3 4))

;; &optional inside a nested pattern, default fires.
(def-op nested-interp (:opt-pair (a &optional (b 99)) c)
  (list a b c))

(check "nested &optional default fires"
       (lower nested-interp '(:opt-pair (10) 30))
       '(10 99 30))

(check "nested &optional value provided"
       (lower nested-interp '(:opt-pair (10 20) 30))
       '(10 20 30))

;; &rest as a nested pattern itself: destructure the entire arg tail.
(def-op nested-interp (:rest-pair &rest (x y &rest tail))
  (list x y tail))

(check "nested &rest pattern"
       (lower nested-interp '(:rest-pair 1 2 3 4 5))
       '(1 2 (3 4 5)))

;; recurse still works across a nested binding -- the rule destructures
;; a binary "node" and lowers each side.
(defvar node-eval (make-interpreter))
(def-op node-eval (:add a b) (+ (recurse a) (recurse b)))
(def-op node-eval (:node (left right))
  (+ (recurse left) (recurse right)))

(check "recurse through nested binding"
       (lower node-eval '(:node ((:add 3 4) (:add 5 3))))
       15)

;; Mismatch errors at nested positions still signal MALFORMED-OPERATOR-ARGS,
;; with the original top-level operator/expression/pattern preserved.
(handler-case
    (progn (lower nested-interp '(:pair 5 30))
           (format t "  FAIL  atom-where-list did not error~%")
           (push '(:fail :no-error) *results*))
  (malformed-operator-args (c)
    (format t "  PASS  atom-where-list signals malformed-operator-args~%")
    (push '(:pass :error-type) *results*)
    (check "  nested-mismatch operator"   (malformed-operator-args-operator c)   :pair)
    (check "  nested-mismatch expression" (malformed-operator-args-expression c) '(:pair 5 30))
    (check "  nested-mismatch pattern"    (malformed-operator-args-pattern c)    '(:pair (a b) c))))

(check-malformed "nested list too short"
                 (lower nested-interp '(:pair (10) 30)))
(check-malformed "nested list too long (no &rest in pattern)"
                 (lower nested-interp '(:pair (10 20 99) 30)))

;; Parser-time error: nested patterns are rejected under &key.
(check "parse error on nested pattern under &key"
       (handler-case
           (progn (macroexpand-1
                   '(def-op nested-interp (:bad &key ((a b) 0))
                     (list a b)))
                  :no-error)
         (error () :parse-error))
       :parse-error)

;;; --- Statement splicing ------------------------------------------------
;;;
;;; A handler can return a SPLICE wrapper to expand one AST node into a
;;; sequence of nodes. Whether the splice is *legal* depends on the call
;;; site -- not the rule. RECURSE-SPLICE invokes a child in splice context
;;; and always returns a list; RECURSE invokes in expression context (the
;;; default) and a splice return signals SPLICE-IN-EXPRESSION-CONTEXT.

(format t "~%=== Statement splicing ===~%")

;; The c-lower interpreter from interpreter.lisp models a C-style
;; declare-and-assign sugar. (:set (type var) val) splices to two
;; statements; :block invokes children with RECURSE-SPLICE; :if uses
;; RECURSE so a splice in any of its slots is rejected.

(check ":block splices :set into two statements"
       (lower c-lower '(:block (:set (:int x) 2)))
       '(:block (:var :int x) (:set x 2)))

(check ":block flattens multiple splices and single-node children"
       (lower c-lower '(:block (:set (:int x) 2)
                               (:set (:int y) 3)
                               (:noop)))
       '(:block (:var :int x) (:set x 2) (:var :int y) (:set y 3) (:noop)))

;; Empty splice (the rule returns no nodes) drops the original node.
(defvar drop-interp (make-interpreter :on-unknown :passthrough))
(def-op drop-interp (:block &rest stmts)
  (cons :block (mapcan #'recurse-splice stmts)))
(def-op drop-interp (:dead) (splice))

(check "empty splice drops the node"
       (lower drop-interp '(:block (:dead) (:other) (:dead)))
       '(:block (:other)))

;; A handler returning a non-splice value is wrapped to a one-element list
;; in splice context -- so :block can mix splicing and non-splicing rules.
(defvar wrap-interp (make-interpreter :on-unknown :passthrough))
(def-op wrap-interp (:block &rest stmts)
  (cons :block (mapcan #'recurse-splice stmts)))
(def-op wrap-interp (:plain) (list :plain-result))

(check "single value wrapped in splice context"
       (lower wrap-interp '(:block (:plain) (:plain)))
       '(:block (:plain-result) (:plain-result)))

;; Atoms (numbers, symbols, strings) lower to themselves and pass through
;; splice context as a one-element list.
(check "atoms pass through splice context as one-element lists"
       (lower wrap-interp '(:block 1 :sym "s"))
       '(:block 1 :sym "s"))

;; Splice in expression context -- structured error condition.
(handler-case
    (progn (lower c-lower '(:if (:set (:int x) 2) :t :f))
           (format t "  FAIL  splice-in-expr did not error~%")
           (push '(:fail :no-error) *results*))
  (splice-in-expression-context (c)
    (format t "  PASS  splice in expression slot signals splice-in-expression-context~%")
    (push '(:pass :error-type) *results*)
    (check "  splice-in-expr operator"
           (splice-in-expression-context-operator c) :set)
    (check "  splice-in-expr node count"
           (splice-in-expression-context-node-count c) 2)
    (check "  splice-in-expr expression"
           (splice-in-expression-context-expression c) '(:set (:int x) 2))))

;; Cross-interpreter splice via the explicit :splice keyword on LOWER.
(defvar producer (make-interpreter :on-unknown :passthrough))
(def-op producer (:two) (splice 'a 'b))

(check "lower with :splice t unwraps splice into a list"
       (lower producer '(:two) :splice t)
       '(a b))

(check "lower without :splice errors on splice return"
       (handler-case (progn (lower producer '(:two)) :no-error)
         (splice-in-expression-context () :errored))
       :errored)

;; Atoms in splice context.
(check "atom in splice context wraps to one-element list"
       (lower producer 42 :splice t)
       '(42))

;; Original behavior is preserved: a non-splicing rule in expression
;; context returns a single value as before, untouched by the new path.
(check "non-splice rule unchanged in expression context"
       (lower arith-eval '(:add 3 4))
       7)

;;; --- Fresh names ------------------------------------------------------
;;;
;;; FRESH-NAME mints uninterned symbols that won't collide with anything
;;; the user wrote. It's the primitive a rule reaches for when its
;;; expansion needs to introduce a binding -- the moral equivalent of
;;; CL's GENSYM, scoped for use inside def-op bodies.

(format t "~%=== Fresh names ===~%")

(check "fresh-name returns an uninterned symbol"
       (symbol-package (fresh-name))
       nil)

(check "fresh-name returns a symbol"
       (symbolp (fresh-name))
       t)

;; Two consecutive calls return distinct symbols.
(let ((a (fresh-name)) (b (fresh-name)))
  (check "consecutive fresh-name calls are not eq"
         (eq a b)
         nil)
  (check "consecutive fresh-name calls have distinct names"
         (string= (symbol-name a) (symbol-name b))
         nil))

;; The prefix is honored.
(check "fresh-name with string prefix"
       (let ((sym (fresh-name "tmp")))
         (and (>= (length (symbol-name sym)) 3)
              (string= (subseq (symbol-name sym) 0 3) "tmp")))
       t)

(check "fresh-name with symbol prefix coerces to name"
       (let ((sym (fresh-name 'loop-end)))
         (and (>= (length (symbol-name sym)) 8)
              (string= (subseq (symbol-name sym) 0 8) "LOOP-END")))
       t)

;; The :swap rule from interpreter.lisp uses fresh-name. Its expansion
;; should produce four statements where the temporary's name is uninterned
;; and not eq to any user-supplied symbol.
(let ((expansion (lower c-lower '(:block (:swap a b)))))
  (check ":swap expands to a 5-element block (block + 4 stmts)"
         (length expansion)
         5)
  (check ":swap's first stmt is :var :auto <tmp>"
         (and (eq (first  (second expansion)) :var)
              (eq (second (second expansion)) :auto))
         t)
  ;; The temporary is uninterned and shared across the four statements.
  (let ((tmp (third (second expansion))))
    (check "the introduced tmp is uninterned"
           (symbol-package tmp)
           nil)
    (check "tmp is reused across :set tmp a and :set b tmp"
           (and (eq tmp (second (third expansion)))  ; (:set tmp a) -> tmp
                (eq tmp (third  (fifth expansion)))) ; (:set b tmp) -> tmp
           t)
    ;; Hygiene check: a user-written symbol named TMP must NOT be eq to
    ;; the introduced temporary, even if the user wrote (:swap tmp other).
    (let ((capture-test (lower c-lower '(:block (:swap tmp other)))))
      (let ((introduced-tmp (third (second capture-test)))
            (user-tmp 'tmp))
        (check "hygiene: introduced tmp is not eq to user's TMP symbol"
               (eq introduced-tmp user-tmp)
               nil)))))

;; Each fresh expansion gets its own temporary -- two :swaps in one block
;; produce two distinct temporaries.
(let ((expansion (lower c-lower '(:block (:swap a b) (:swap c d)))))
  (let ((tmp-1 (third (second expansion)))   ; first :swap's :var
        (tmp-2 (third (sixth expansion))))   ; second :swap's :var
    (check "two :swap expansions produce distinct temporaries"
           (eq tmp-1 tmp-2)
           nil)))


;;; --- Local rule overrides --------------------------------------------
;;;
;;; WITH-OVERRIDES installs handlers on an interpreter for the dynamic
;;; extent of a body, restoring the originals on any exit (normal or
;;; non-local). The CL analog is MACROLET; the use cases are tests,
;;; one-shot experiments, and passes that want a sub-tree lowered with
;;; a tweaked rule set.

(format t "~%=== Local rule overrides ===~%")

;; Setup: an arithmetic interpreter we'll override against.
(defvar override-interp (make-interpreter))
(def-op override-interp (:add a b) (+ (recurse a) (recurse b)))
(def-op override-interp (:mul a b) (* (recurse a) (recurse b)))

(check "baseline: :add returns sum"
       (lower override-interp '(:add 3 4))
       7)

;; Simple override.
(check "with-overrides changes behavior inside body"
       (with-overrides (override-interp
                        ((:add a b) (- (recurse a) (recurse b))))
         (lower override-interp '(:add 10 3)))
       7)  ; 10 - 3 with the overridden rule

;; Original rule is restored after body.
(check "original rule restored after with-overrides exits"
       (progn
         (with-overrides (override-interp
                          ((:add a b) (- (recurse a) (recurse b))))
           (lower override-interp '(:add 10 3)))
         (lower override-interp '(:add 10 3)))
       13)

;; Restoration on non-local exit.
(check "with-overrides restores on error"
       (progn
         (handler-case
             (with-overrides (override-interp
                              ((:add a b) (- (recurse a) (recurse b))))
               (error "oops"))
           (error () nil))
         (lower override-interp '(:add 10 3)))
       13)

;; Overriding an operator that wasn't previously defined: REMHASH on exit.
(check "previously-undefined operator is removed on exit"
       (progn
         (with-overrides (override-interp
                          ((:noop) :a-result))
           (lower override-interp '(:noop)))
         ;; Outside the body, :noop should be undefined again.
         (handler-case (progn (lower override-interp '(:noop)) :still-defined)
           (unknown-operator () :gone)))
       :gone)

;; Nesting: inner override shadows outer; on inner exit, outer is restored;
;; on outer exit, the original is restored.
(check "nested overrides: inner takes precedence"
       (with-overrides (override-interp
                        ((:add a b) (- (recurse a) (recurse b))))
         (with-overrides (override-interp
                          ((:add a b) (* (recurse a) (recurse b))))
           (lower override-interp '(:add 4 5))))
       20)  ; innermost is multiplication

(check "nested overrides: outer restored after inner exits"
       (with-overrides (override-interp
                        ((:add a b) (- (recurse a) (recurse b))))
         (with-overrides (override-interp
                          ((:add a b) (* (recurse a) (recurse b))))
           (lower override-interp '(:add 4 5)))
         (lower override-interp '(:add 10 3)))
       7)  ; outer is subtraction

(check "nested overrides: original restored after both exit"
       (progn
         (with-overrides (override-interp
                          ((:add a b) (- (recurse a) (recurse b))))
           (with-overrides (override-interp
                            ((:add a b) (* (recurse a) (recurse b))))
             (lower override-interp '(:add 4 5))))
         (lower override-interp '(:add 10 3)))
       13)  ; baseline addition

;; Overrides with recurse: an override can call other rules on the
;; interpreter, including the rules it's NOT overriding.
(check "override body can call recurse on non-overridden rules"
       (with-overrides (override-interp
                        ((:add a b)
                         ;; Use :mul for the computation; :mul is unchanged.
                         (recurse (list :mul a b))))
         (lower override-interp '(:add 4 5)))
       20)

;; Overrides support the full lambda-list machinery: nested patterns,
;; &optional, &rest, splice, fresh-name -- everything DEF-OP supports.
(check "overrides support nested patterns"
       (with-overrides (override-interp
                        ((:add (a b) c)
                         (+ (recurse a) (recurse b) (recurse c))))
         (lower override-interp '(:add (10 20) 30)))
       60)


;;; --- Tracing ---------------------------------------------------------
;;;
;;; WITH-TRACE returns (values RESULT TRACE) where TRACE is a list of
;;; TRACE-ENTRY trees -- one per top-level LOWER call performed inside
;;; the body. Each entry records what LOWER saw, what fired, and what
;;; came out, with children mirroring recursive lower calls.

(format t "~%=== Tracing ===~%")

;; Without with-trace, lower is unchanged.
(check "tracing inactive: lower returns just one value"
       (lower arith-eval '(:add 1 2))
       3)

;; with-trace returns two values.
(multiple-value-bind (result trace)
    (with-trace () (lower arith-eval '(:add 3 4)))
  (check "with-trace result preserved"
         result 7)
  (check "with-trace trace is a list"
         (listp trace) t)
  (check "with-trace produces one root entry per top-level call"
         (length trace) 1)
  (let ((root (first trace)))
    (check "root entry input"
           (trace-entry-input root) '(:add 3 4))
    (check "root entry operator"
           (trace-entry-operator root) :add)
    (check "root entry handler-p"
           (trace-entry-handler-p root) t)
    (check "root entry output"
           (trace-entry-output root) 7)
    ;; The :add rule recursively lowers its two arguments.
    (check "root has two children (one per argument)"
           (length (trace-entry-children root)) 2)
    (let ((c0 (first  (trace-entry-children root)))
          (c1 (second (trace-entry-children root))))
      (check "first child is the literal 3"
             (trace-entry-input c0) 3)
      (check "first child has no operator"
             (trace-entry-operator c0) nil)
      (check "first child handler-p is nil (atom)"
             (trace-entry-handler-p c0) nil)
      (check "first child output is 3"
             (trace-entry-output c0) 3)
      (check "second child input is 4"
             (trace-entry-input c1) 4))))

;; Nested call: trace tree mirrors the AST.
(multiple-value-bind (result trace)
    (with-trace () (lower arith-eval '(:add (:mul 2 3) 4)))
  (check "nested call result"
         result 10)
  (let* ((root (first trace))
         (mul  (first (trace-entry-children root))))
    (check "nested: root op is :add"
           (trace-entry-operator root) :add)
    (check "nested: first child op is :mul"
           (trace-entry-operator mul) :mul)
    (check "nested: :mul has two literal children"
           (length (trace-entry-children mul)) 2)
    (check "nested: :mul output is 6"
           (trace-entry-output mul) 6)))

;; Multiple top-level calls inside one with-trace: one entry per call.
(multiple-value-bind (result trace)
    (with-trace ()
      (list (lower arith-eval '(:add 1 2))
            (lower arith-eval '(:mul 3 4))))
  (check "multiple top-level calls: result"
         result '(3 12))
  (check "multiple top-level calls: trace length"
         (length trace) 2)
  (check "multiple top-level calls: ops in order"
         (mapcar #'trace-entry-operator trace)
         '(:add :mul)))

;; Splice context shows up properly: the :block trace has one child per
;; recursed-on stmt, regardless of how many output nodes each produced.
(multiple-value-bind (result trace)
    (with-trace () (lower c-lower '(:block (:set (:int x) 2)
                                           (:noop))))
  (declare (ignore result))
  (let ((block-entry (first trace)))
    (check "splice trace: :block has children for each input stmt"
           (length (trace-entry-children block-entry)) 2)
    (let ((set-entry (first (trace-entry-children block-entry))))
      (check "splice trace: :set entry recorded"
             (trace-entry-operator set-entry) :set)
      ;; Regression test: the :set's recorded splice output must not be
      ;; mutated by the parent's mapcan, which destructively concatenates.
      (check "splice trace: :set output is a 2-element splice snapshot"
             (length (trace-entry-output set-entry)) 2))))

;; Tracing is off after with-trace returns -- no entries leak.
(check "tracing is local: *trace-stack* nil outside with-trace"
       (progn (with-trace () (lower arith-eval '(:add 1 2)))
              *trace-stack*)
       nil)


(format t "~%=== Summary ===~%")
(let ((pass (count :pass *results* :key #'car))
      (fail (count :fail *results* :key #'car)))
  (format t "  ~D passed, ~D failed~%" pass fail)
  (values pass fail))
