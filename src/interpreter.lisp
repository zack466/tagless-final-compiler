(in-package #:tagless-compiler)

;; --- Conditions and restarts ---

(define-condition unknown-operator (error)
  ((operator    :initarg :operator    :reader unknown-operator-operator)
   (expression  :initarg :expression  :reader unknown-operator-expression)
   (interpreter :initarg :interpreter :reader unknown-operator-interpreter))
  (:report (lambda (c stream)
             (format stream "No handler defined for operator ~S in interpreter ~S~@
                             (expression: ~S)"
                     (unknown-operator-operator c)
                     (unknown-operator-interpreter c)
                     (unknown-operator-expression c)))))

(define-condition malformed-operator-args (error)
  ((operator   :initarg :operator   :reader malformed-operator-args-operator)
   (expression :initarg :expression :reader malformed-operator-args-expression)
   (pattern    :initarg :pattern    :initform nil
               :reader malformed-operator-args-pattern))
  (:report (lambda (c stream)
             (format stream
                     "Arguments to operator ~S do not match its expected shape.~@
                      Expression: ~S~@
                      Expected pattern: ~S"
                     (malformed-operator-args-operator c)
                     (malformed-operator-args-expression c)
                     (malformed-operator-args-pattern c)))))

(defun passthrough-unknown-operator (condition)
  "Handler that resolves UNKNOWN-OPERATOR by returning the expression unchanged.
   Invokes the USE-VALUE restart with the original expression."
  (let ((restart (find-restart 'use-value condition)))
    (when restart
      (invoke-restart restart (unknown-operator-expression condition)))))

(defun recurse-unknown-operator (condition)
  "Handler that resolves UNKNOWN-OPERATOR by recursing into each element of
   the sub-expression's body. Rebuilds the expression with the original head
   (unknown operator) but with each argument lowered individually.
   Invokes the USE-VALUE restart with the rebuilt expression."
  (let ((restart (find-restart 'use-value condition)))
    (when restart
      (let* ((expr   (unknown-operator-expression condition))
             (interp (unknown-operator-interpreter condition))
             (head   (first expr))
             (args   (rest expr))
             (rebuilt (cons head
                            (mapcar (lambda (arg) (lower interp arg)) args))))
        (invoke-restart restart rebuilt)))))

;; --- Interpreter ---

(defclass interpreter ()
  ((handlers :initform (make-hash-table :test #'eq)
             :reader   handlers
             :documentation "Hash table mapping keywords to handler functions.")
   (on-unknown :initarg :on-unknown
               :initform :error
               :accessor on-unknown
               :documentation
               "What to do when an operator has no handler.
                :error       -- signal UNKNOWN-OPERATOR (default)
                :passthrough -- return the sub-expression unchanged
                :recurse     -- recurse into arguments, keep head intact
                a function   -- called with the condition; typically invokes
                                a restart (USE-VALUE) or returns normally
                                to fall through to :error behavior.")))

(defun make-interpreter (&key (on-unknown :error))
  (make-instance 'interpreter :on-unknown on-unknown))

; generic 'lower' function to enable recursion, dispatched on type of interpreter
(defgeneric lower (interpreter expression)
  (:documentation "Interprets the EXPRESSION using the rules defined in INTERPRETER."))

; "interprets" a cons cell by dispatching on its head symbol
(defmethod lower ((interp interpreter) (expr cons))
  "Interprets a list by looking up the operator keyword in the interpreter's table.
   If no handler is found, signals UNKNOWN-OPERATOR. A USE-VALUE restart is
   available to substitute a replacement value for the sub-expression."
  (let ((op (first expr)))
    (cond
      ((not (keywordp op))
       (error "Interpreter expects keyword operators, got ~S" op))
      (t
       (let ((handler (gethash op (handlers interp))))
         (if handler
             (funcall handler expr)
             (signal-unknown-operator interp op expr)))))))

; Fallback when interpreter is not defined on the symbol, just return unchanged
(defmethod lower ((interp interpreter) (expr t))
  expr)

(defun signal-unknown-operator (interp op expr)
  "Signal an UNKNOWN-OPERATOR condition, offering USE-VALUE as a restart.
   Behavior when no outer handler intervenes is controlled by the interpreter's
   ON-UNKNOWN slot."
  (restart-case
      (let ((condition (make-condition 'unknown-operator
                                       :operator op
                                       :expression expr
                                       :interpreter interp)))
        ;; If the interpreter has a default policy, install it as an inner
        ;; handler so that user-provided outer handlers take precedence.
        (handler-bind
            ((unknown-operator
               (lambda (c)
                 (apply-unknown-policy (on-unknown interp) c))))
          (error condition)))
    (use-value (value)
      :report "Use a replacement value for this sub-expression."
      :interactive (lambda ()
                     (format *query-io* "~&Replacement value: ")
                     (list (read *query-io*)))
      value)))

(defun apply-unknown-policy (policy condition)
  "Apply the interpreter's ON-UNKNOWN policy to CONDITION."
  (cond
    ((eq policy :error)
     nil) ; decline; the ERROR call will propagate
    ((eq policy :passthrough)
     (passthrough-unknown-operator condition))
    ((eq policy :recurse)
     (recurse-unknown-operator condition))
    ((functionp policy)
     (funcall policy condition))
    (t
     (error "Invalid ON-UNKNOWN policy: ~S" policy))))

;; --- def-op ---
;;
;; Defines a handler for a specific interpreter instance. CLAUSE is
;; (KEYWORD . LAMBDA-LIST) -- e.g. (:add a b), (:scale x &key (by 1)),
;; or (:call fn &rest args). Inside BODY:
;;
;;   * Every variable in the lambda list is in scope as a normal lexical
;;     binding.
;;
;;   * RECURSE is bound to a one-argument local function that lowers a
;;     sub-expression with the *current* interpreter -- the common case.
;;
;;   * The generic LOWER is unshadowed, so a rule that wants to dispatch
;;     to a *different* interpreter can call (LOWER OTHER-INTERP X)
;;     directly. This is the escape hatch for multi-interpreter pipelines
;;     (e.g. constant folding before pretty-printing).
;;
;; The expansion is small: it parses the lambda list at compile time, builds
;; the runtime arguments (with default forms wrapped in thunks), and emits a
;; handler that delegates to MATCH-LAMBDA-LIST and DESTRUCTURING-BINDs the
;; returned values to the lambda-list variable names. Lexical bindings mean
;; CL-package symbols (TYPE, COUNT, LIST, ...) are usable as parameter names
;; without tripping package locks.

(defmacro def-op (interp clause &body body)
  (destructuring-bind (keyword &rest lambda-list) clause
    (unless (keywordp keyword)
      (error "def-op: clause head must be a keyword, got ~S" keyword))
    (multiple-value-bind (positionals optionals rest-var key-specs)
        (parse-lambda-list lambda-list)
      (let ((all-vars (append positionals
                              (mapcar #'first optionals)
                              (and rest-var (list rest-var))
                              (mapcar #'second key-specs)))
            (expr (gensym "EXPR")))
        (flet ((thunk (form)
                 ;; NIL means "no default" -- the matcher checks for this.
                 (if form `(lambda () ,form) nil)))
          `(setf (gethash ,keyword (handlers ,interp))
                 (lambda (,expr)
                   (destructuring-bind ,all-vars
                       (match-lambda-list
                        ,(length positionals)
                        (list ,@(loop for (var default) in optionals
                                      collect `(list ',var ,(thunk default))))
                        ',rest-var
                        (list ,@(loop for (kw var default) in key-specs
                                      collect `(list ',kw ',var ,(thunk default))))
                        (rest ,expr)
                        ,keyword
                        ,expr
                        ',clause)
                     (flet ((recurse (x) (lower ,interp x)))
                       ,@body)))))))))

;; --- Convenience: one-shot handlers without mutating the interpreter ---

(defmacro with-passthrough-unknown (() &body body)
  "Execute BODY with UNKNOWN-OPERATOR conditions resolved by returning the
   original expression unchanged."
  `(handler-bind ((unknown-operator #'passthrough-unknown-operator))
     ,@body))

(defmacro with-recurse-unknown (() &body body)
  "Execute BODY with UNKNOWN-OPERATOR conditions resolved by recursing into
   the arguments of the sub-expression while preserving the unknown head."
  `(handler-bind ((unknown-operator #'recurse-unknown-operator))
     ,@body))


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

;; --- Evaluation ---

(defvar *program* '(:mul (:add 1 2) (:inc 5)))

(lower arith-eval *program*)
;; => 18

(lower string-repr *program*)
;; => "(Mul (Add 1 2) (Inc 5))"

;; Unknown ops now have their bodies recursed into:
(lower partial-rewrite '(:if (:square 2) (:square 3) (:add (:square 4) 1)))
;; => (:if (:mul 2 2) (:mul 3 3) (:add (:mul 4 4) 1))
