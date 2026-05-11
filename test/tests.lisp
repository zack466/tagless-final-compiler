;;;; tests.lisp — Tests for the tagless-final interpreter framework.
;;;;
;;;; Requires the tagless-compiler system (loads interpreter, example, etc.)
;;;; and the harness (loaded first by ASDF). Each DEFSUITE runs at load time.

(in-package #:tagless-compiler)

;;; ---------------------------------------------------------------------------
;;; Setup: test-local interpreters (must be at top-level, outside defsuite)
;;; ---------------------------------------------------------------------------

;;; &rest support
(defvar rest-interp (make-interpreter))
(def-op rest-interp (:sum &rest xs)
  (reduce #'+ (mapcar #'recurse xs) :initial-value 0))

;;; &key support (with defaults)
(defvar key-interp (make-interpreter))
(def-op key-interp (:scale x &key (by 1) (offset 0))
  (+ (* (recurse x) by) offset))

;;; &optional support (with defaults)
(defvar opt-interp (make-interpreter))
(def-op opt-interp (:maybe-inc x &optional (n 1))
  (+ (recurse x) n))

;;; Mixed: required + &rest
(defvar mixed-interp (make-interpreter))
(def-op mixed-interp (:cons-up head &rest tail)
  (cons (recurse head) (mapcar #'recurse tail)))

;;; Body error propagation
(defvar body-err-interp (make-interpreter))
(def-op body-err-interp (:divide a b)
  (/ (recurse a) (recurse b)))

;;; CL-package symbol parameter names
(defvar cl-name-interp (make-interpreter))
(def-op cl-name-interp (:declare-var name type)
  (format nil "var ~A : ~A" (recurse name) (recurse type)))
(def-op cl-name-interp (:counted &optional (count 0))
  (recurse count))
(def-op cl-name-interp (:listing &rest list)
  (length list))

;;; Cross-interpreter dispatch
(defvar fold-interp (make-interpreter :on-unknown :recurse))
(def-op fold-interp (:add a b)
  (let ((aa (recurse a)) (bb (recurse b)))
    (if (and (numberp aa) (numberp bb)) (+ aa bb) (list :add aa bb))))

(defvar print-interp (make-interpreter))
(def-op print-interp (:add a b)
  (format nil "(~A + ~A)" (recurse a) (recurse b)))
(def-op print-interp (:embed-folded x)
  (format nil "[folded: ~A]" (lower fold-interp x)))

;;; Nested patterns
(defvar nested-interp (make-interpreter))
(def-op nested-interp (:pair (a b) c)
  (list a b c))
(def-op nested-interp (:deep ((a b) c) d)
  (list a b c d))
(def-op nested-interp (:opt-pair (a &optional (b 99)) c)
  (list a b c))
(def-op nested-interp (:rest-pair &rest (x y &rest tail))
  (list x y tail))

(defvar node-eval (make-interpreter))
(def-op node-eval (:add a b) (+ (recurse a) (recurse b)))
(def-op node-eval (:node (left right))
  (+ (recurse left) (recurse right)))

;;; Statement splicing helpers
(defvar drop-interp (make-interpreter :on-unknown :passthrough))
(def-op drop-interp (:block &rest stmts)
  (cons :block (mapcan #'recurse-splice stmts)))
(def-op drop-interp (:dead) (splice))

(defvar wrap-interp (make-interpreter :on-unknown :passthrough))
(def-op wrap-interp (:block &rest stmts)
  (cons :block (mapcan #'recurse-splice stmts)))
(def-op wrap-interp (:plain) (list :plain-result))

(defvar producer (make-interpreter :on-unknown :passthrough))
(def-op producer (:two) (splice 'a 'b))

;;; Override tests
(defvar override-interp (make-interpreter))
(def-op override-interp (:add a b) (+ (recurse a) (recurse b)))
(def-op override-interp (:mul a b) (* (recurse a) (recurse b)))

;;; ---------------------------------------------------------------------------
;;; Test suites
;;; ---------------------------------------------------------------------------

(defsuite "original behavior"
  (deftest "arith-eval on *program*"
    (check (lower arith-eval *program*) 18))

  (deftest "string-repr on *program*"
    (check (lower string-repr *program*) "(Mul (Add 1 2) (Inc 5))"))

  (deftest "partial-rewrite recurses into unknown heads"
    (check (lower partial-rewrite '(:if (:square 2) (:square 3) (:add (:square 4) 1)))
           '(:if (:mul 2 2) (:mul 3 3) (:add (:mul 4 4) 1)))))

(defsuite "def-op lambda-list features"
  (deftest ":sum with &rest, 0 args"  (check (lower rest-interp '(:sum)) 0))
  (deftest ":sum with &rest, 1 arg"   (check (lower rest-interp '(:sum 5)) 5))
  (deftest ":sum with &rest, 4 args"  (check (lower rest-interp '(:sum 1 2 3 4)) 10))

  (deftest ":scale with defaults"
    (check (lower key-interp '(:scale 10)) 10))
  (deftest ":scale with :by"
    (check (lower key-interp '(:scale 10 :by 3)) 30))
  (deftest ":scale with :by :offset"
    (check (lower key-interp '(:scale 10 :by 3 :offset 5)) 35))

  (deftest ":maybe-inc default"
    (check (lower opt-interp '(:maybe-inc 5)) 6))
  (deftest ":maybe-inc explicit"
    (check (lower opt-interp '(:maybe-inc 5 10)) 15))

  (deftest ":cons-up mixed"
    (check (lower mixed-interp '(:cons-up 1 2 3 4)) '(1 2 3 4))))

(defsuite "malformed-operator-args condition"
  (deftest "malformed :add signals malformed-operator-args with correct slots"
    (handler-case
        (progn (lower arith-eval '(:add 1))
               (error "expected signal"))
      (malformed-operator-args (c)
        (check (malformed-operator-args-operator c) :add)
        (check (malformed-operator-args-expression c) '(:add 1))
        (check (malformed-operator-args-pattern c) '(:add a b)))))

  (deftest "outer handler can recover"
    (check (handler-case (lower arith-eval '(:add 1))
             (malformed-operator-args (c) (declare (ignore c)) :recovered))
           :recovered))

  (deftest "body errors propagate as themselves (not malformed-operator-args)"
    (check (handler-case (lower body-err-interp '(:divide 1 0))
             (malformed-operator-args () :wrongly-caught)
             (division-by-zero ()        :correctly-propagated)
             (arithmetic-error ()        :correctly-propagated))
           :correctly-propagated))

  (deftest "too few positional args"
    (check-error (lower arith-eval '(:add 1)) malformed-operator-args))
  (deftest "too many positional args"
    (check-error (lower arith-eval '(:add 1 2 3)) malformed-operator-args))
  (deftest "odd-length keyword block"
    (check-error (lower key-interp '(:scale 10 :by)) malformed-operator-args))
  (deftest "unknown keyword in &key block"
    (check-error (lower key-interp '(:scale 10 :nope 5)) malformed-operator-args))
  (deftest "trailing junk with no &rest"
    (check-error (lower key-interp '(:scale 10 :by 3 99)) malformed-operator-args))

  (deftest "parse error on &rest without var"
    (check (handler-case
               (progn (macroexpand-1 '(def-op key-interp (:bad &rest))) :no-error)
             (error () :parse-error))
           :parse-error))

  (deftest "parse error on &optional after &key"
    (check (handler-case
               (progn (macroexpand-1 '(def-op key-interp (:bad &key x &optional y)))
                      :no-error)
             (error () :parse-error))
           :parse-error)))

(defsuite "CL-package symbols as parameter names"
  (deftest "cl:type as parameter name"
    (check (lower cl-name-interp '(:declare-var :x :int)) "var X : INT"))

  (deftest "cl:count as &optional name (default)"
    (check (lower cl-name-interp '(:counted)) 0))
  (deftest "cl:count as &optional name (provided)"
    (check (lower cl-name-interp '(:counted 7)) 7))

  (deftest "cl:list as &rest name"
    (check (lower cl-name-interp '(:listing 1 2 3 4)) 4)))

(defsuite "cross-interpreter dispatch"
  (deftest "self-recurse via RECURSE"
    (check (lower print-interp '(:add 1 (:add 2 3)))
           "(1 + (2 + 3))"))
  (deftest "cross-dispatch via generic LOWER"
    (check (lower print-interp '(:add 1 (:embed-folded (:add 2 (:add 3 4)))))
           "(1 + [folded: 9])")))

(defsuite "nested patterns"
  (deftest "nested positional list"
    (check (lower nested-interp '(:pair (10 20) 30)) '(10 20 30)))

  (deftest "doubly nested positional"
    (check (lower nested-interp '(:deep ((1 2) 3) 4)) '(1 2 3 4)))

  (deftest "nested &optional default fires"
    (check (lower nested-interp '(:opt-pair (10) 30)) '(10 99 30)))
  (deftest "nested &optional value provided"
    (check (lower nested-interp '(:opt-pair (10 20) 30)) '(10 20 30)))

  (deftest "nested &rest pattern"
    (check (lower nested-interp '(:rest-pair 1 2 3 4 5)) '(1 2 (3 4 5))))

  (deftest "recurse through nested binding"
    (check (lower node-eval '(:node ((:add 3 4) (:add 5 3)))) 15))

  (deftest "atom-where-list signals malformed-operator-args with correct slots"
    (handler-case
        (progn (lower nested-interp '(:pair 5 30))
               (error "expected signal"))
      (malformed-operator-args (c)
        (check (malformed-operator-args-operator c) :pair)
        (check (malformed-operator-args-expression c) '(:pair 5 30))
        (check (malformed-operator-args-pattern c) '(:pair (a b) c)))))

  (deftest "nested list too short"
    (check-error (lower nested-interp '(:pair (10) 30)) malformed-operator-args))
  (deftest "nested list too long (no &rest in pattern)"
    (check-error (lower nested-interp '(:pair (10 20 99) 30)) malformed-operator-args))

  (deftest "parse error on nested pattern under &key"
    (check (handler-case
               (progn (macroexpand-1
                       '(def-op nested-interp (:bad &key ((a b) 0)) (list a b)))
                      :no-error)
             (error () :parse-error))
           :parse-error)))

(defsuite "statement splicing"
  (deftest ":block splices :set into two statements"
    (check (lower c-lower '(:block (:set (:int x) 2)))
           '(:block (:var :int x) (:set x 2))))

  (deftest ":block flattens multiple splices and single-node children"
    (check (lower c-lower '(:block (:set (:int x) 2)
                                   (:set (:int y) 3)
                                   (:noop)))
           '(:block (:var :int x) (:set x 2) (:var :int y) (:set y 3) (:noop))))

  (deftest "empty splice drops the node"
    (check (lower drop-interp '(:block (:dead) (:other) (:dead)))
           '(:block (:other))))

  (deftest "single value wrapped in splice context"
    (check (lower wrap-interp '(:block (:plain) (:plain)))
           '(:block (:plain-result) (:plain-result))))

  (deftest "atoms pass through splice context as one-element lists"
    (check (lower wrap-interp '(:block 1 :sym "s"))
           '(:block 1 :sym "s")))

  (deftest "splice in expression slot signals splice-in-expression-context with correct slots"
    (handler-case
        (progn (lower c-lower '(:if (:set (:int x) 2) :t :f))
               (error "expected signal"))
      (splice-in-expression-context (c)
        (check (splice-in-expression-context-operator c) :set)
        (check (splice-in-expression-context-node-count c) 2)
        (check (splice-in-expression-context-expression c) '(:set (:int x) 2)))))

  (deftest "lower with :splice t unwraps splice into a list"
    (check (lower producer '(:two) :splice t) '(a b)))

  (deftest "lower without :splice errors on splice return"
    (check (handler-case (progn (lower producer '(:two)) :no-error)
             (splice-in-expression-context () :errored))
           :errored))

  (deftest "atom in splice context wraps to one-element list"
    (check (lower producer 42 :splice t) '(42)))

  (deftest "non-splice rule unchanged in expression context"
    (check (lower arith-eval '(:add 3 4)) 7)))

(defsuite "fresh names"
  (deftest "fresh-name returns an uninterned symbol"
    (check (symbol-package (fresh-name)) nil))

  (deftest "fresh-name returns a symbol"
    (check-true (symbolp (fresh-name))))

  (deftest "consecutive fresh-name calls are distinct"
    (let ((a (fresh-name)) (b (fresh-name)))
      (check-false (eq a b))
      (check-false (string= (symbol-name a) (symbol-name b)))))

  (deftest "fresh-name with string prefix"
    (let ((sym (fresh-name "tmp")))
      (check-true (and (>= (length (symbol-name sym)) 3)
                       (string= (subseq (symbol-name sym) 0 3) "tmp")))))

  (deftest "fresh-name with symbol prefix coerces to name"
    (let ((sym (fresh-name 'loop-end)))
      (check-true (and (>= (length (symbol-name sym)) 8)
                       (string= (subseq (symbol-name sym) 0 8) "LOOP-END")))))

  (deftest ":swap expands correctly with fresh uninterned tmp"
    (let ((expansion (lower c-lower '(:block (:swap a b)))))
      (check (length expansion) 5)
      (check (and (eq (first  (second expansion)) :var)
                  (eq (second (second expansion)) :auto))
             t)
      (let ((tmp (third (second expansion))))
        (check (symbol-package tmp) nil)
        (check (and (eq tmp (second (third expansion)))
                    (eq tmp (third  (fifth expansion))))
               t)
        ;; Hygiene: user's TMP symbol must differ from introduced tmp.
        (let ((capture-test (lower c-lower '(:block (:swap tmp other)))))
          (check-false (eq (third (second capture-test)) 'tmp))))))

  (deftest "two :swaps produce distinct temporaries"
    (let ((expansion (lower c-lower '(:block (:swap a b) (:swap c d)))))
      (check-false (eq (third (second expansion))
                       (third (sixth expansion)))))))

(defsuite "local rule overrides"
  (deftest "baseline: :add returns sum"
    (check (lower override-interp '(:add 3 4)) 7))

  (deftest "with-overrides changes behavior inside body"
    (check (with-overrides (override-interp
                            ((:add a b) (- (recurse a) (recurse b))))
             (lower override-interp '(:add 10 3)))
           7))

  (deftest "original rule restored after with-overrides exits"
    (progn
      (with-overrides (override-interp
                       ((:add a b) (- (recurse a) (recurse b))))
        (lower override-interp '(:add 10 3)))
      (check (lower override-interp '(:add 10 3)) 13)))

  (deftest "with-overrides restores on error"
    (handler-case
        (with-overrides (override-interp
                         ((:add a b) (- (recurse a) (recurse b))))
          (error "oops"))
      (error () nil))
    (check (lower override-interp '(:add 10 3)) 13))

  (deftest "previously-undefined operator is removed on exit"
    (with-overrides (override-interp ((:noop) :a-result))
      (lower override-interp '(:noop)))
    (check (handler-case (progn (lower override-interp '(:noop)) :still-defined)
             (unknown-operator () :gone))
           :gone))

  (deftest "nested overrides: inner takes precedence"
    (check (with-overrides (override-interp
                            ((:add a b) (- (recurse a) (recurse b))))
             (with-overrides (override-interp
                              ((:add a b) (* (recurse a) (recurse b))))
               (lower override-interp '(:add 4 5))))
           20))

  (deftest "nested overrides: outer restored after inner exits"
    (check (with-overrides (override-interp
                            ((:add a b) (- (recurse a) (recurse b))))
             (with-overrides (override-interp
                              ((:add a b) (* (recurse a) (recurse b))))
               (lower override-interp '(:add 4 5)))
             (lower override-interp '(:add 10 3)))
           7))

  (deftest "nested overrides: original restored after both exit"
    (with-overrides (override-interp
                     ((:add a b) (- (recurse a) (recurse b))))
      (with-overrides (override-interp
                       ((:add a b) (* (recurse a) (recurse b))))
        (lower override-interp '(:add 4 5))))
    (check (lower override-interp '(:add 10 3)) 13))

  (deftest "override body can call recurse on non-overridden rules"
    (check (with-overrides (override-interp
                            ((:add a b)
                             (recurse (list :mul a b))))
             (lower override-interp '(:add 4 5)))
           20))

  (deftest "overrides support nested patterns"
    (check (with-overrides (override-interp
                            ((:add (a b) c)
                             (+ (recurse a) (recurse b) (recurse c))))
             (lower override-interp '(:add (10 20) 30)))
           60)))

(defsuite "tracing"
  (deftest "tracing inactive: lower returns just one value"
    (check (lower arith-eval '(:add 1 2)) 3))

  (deftest "with-trace basic: result and trace structure"
    (multiple-value-bind (result trace)
        (with-trace () (lower arith-eval '(:add 3 4)))
      (check result 7)
      (check-true (listp trace))
      (check (length trace) 1)
      (let ((root (first trace)))
        (check (trace-entry-input root) '(:add 3 4))
        (check (trace-entry-operator root) :add)
        (check (trace-entry-handler-p root) t)
        (check (trace-entry-output root) 7)
        (check (length (trace-entry-children root)) 2)
        (let ((c0 (first  (trace-entry-children root)))
              (c1 (second (trace-entry-children root))))
          (check (trace-entry-input c0) 3)
          (check (trace-entry-operator c0) nil)
          (check (trace-entry-handler-p c0) nil)
          (check (trace-entry-output c0) 3)
          (check (trace-entry-input c1) 4)))))

  (deftest "nested trace mirrors AST"
    (multiple-value-bind (result trace)
        (with-trace () (lower arith-eval '(:add (:mul 2 3) 4)))
      (check result 10)
      (let* ((root (first trace))
             (mul  (first (trace-entry-children root))))
        (check (trace-entry-operator root) :add)
        (check (trace-entry-operator mul) :mul)
        (check (length (trace-entry-children mul)) 2)
        (check (trace-entry-output mul) 6))))

  (deftest "multiple top-level calls produce one entry each"
    (multiple-value-bind (result trace)
        (with-trace ()
          (list (lower arith-eval '(:add 1 2))
                (lower arith-eval '(:mul 3 4))))
      (check result '(3 12))
      (check (length trace) 2)
      (check (mapcar #'trace-entry-operator trace) '(:add :mul))))

  (deftest "splice trace: :block children and splice snapshot"
    (multiple-value-bind (result trace)
        (with-trace () (lower c-lower '(:block (:set (:int x) 2) (:noop))))
      (declare (ignore result))
      (let ((block-entry (first trace)))
        (check (length (trace-entry-children block-entry)) 2)
        (let ((set-entry (first (trace-entry-children block-entry))))
          (check (trace-entry-operator set-entry) :set)
          (check (length (trace-entry-output set-entry)) 2)))))

  (deftest "tracing is local: *trace-stack* nil outside with-trace"
    (with-trace () (lower arith-eval '(:add 1 2)))
    (check *trace-stack* nil)))
