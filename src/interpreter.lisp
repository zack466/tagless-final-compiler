(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

;; --- Conditions and restarts ---

(define-condition unknown-operator (error)
  ((operator    :initarg :operator    :reader unknown-operator-operator)
   (expression  :initarg :expression  :reader unknown-operator-expression)
   (interpreter :initarg :interpreter :reader unknown-operator-interpreter))
  (:report (lambda (c stream)
             ;; Print the error message
             (format stream "~A: " (severity-color :error "Error"))
             (format stream "No handler defined for operator ~A in interpreter ~A~@
                             (expression: ~A)"
                     (lisp-to-string (unknown-operator-operator c))
                     (bold (readable-name (unknown-operator-interpreter c)))
                     (lisp-to-string (unknown-operator-expression c)))
             ;; Print the source context
             (let ((loc (source-loc-or-ancestor
                         (unknown-operator-expression c))))
               (when loc
                 (format stream "~%  at:~%")
                 (print-source-context loc :stream stream))))))

(define-condition malformed-operator-args (error)
  ((operator   :initarg :operator   :reader malformed-operator-args-operator)
   (expression :initarg :expression :reader malformed-operator-args-expression)
   (pattern    :initarg :pattern    :initform nil
               :reader malformed-operator-args-pattern))
  (:report (lambda (c stream)
             (format stream "~A: " (severity-color :error "Error"))
             (format stream
                     "Arguments to operator ~A do not match its expected shape.~@
                      Expression: ~A~@
                      Expected pattern: ~A~@[~%  at ~A~]"
                     (lisp-to-string (malformed-operator-args-operator c))
                     (lisp-to-string (malformed-operator-args-expression c))
                     (lisp-to-string (malformed-operator-args-pattern c))
                     (let ((loc (source-loc-or-ancestor
                                 (malformed-operator-args-expression c))))
                       (when loc
                         (format stream "~%  at:~%")
                         (print-source-context loc :stream stream)))))))

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
   The rebuilt expression inherits its source location from the original
   (first-wins semantics; the outer LOWER will not overwrite it).
   Invokes the USE-VALUE restart with the rebuilt expression."
  (let ((restart (find-restart 'use-value condition)))
    (when restart
      (let* ((expr   (unknown-operator-expression condition))
             (interp (unknown-operator-interpreter condition))
             (head   (first expr))
             (args   (rest expr))
             (rebuilt (inherit-loc
                       (cons head
                             (mapcar (lambda (arg) (lower interp arg)) args))
                       expr)))
        (invoke-restart restart rebuilt)))))

(defun apply-unknown-policy (policy condition)
  "Apply the interpreter's ON-UNKNOWN policy to CONDITION."
  (cond
    ((eq policy :error) nil)
    ((eq policy :passthrough) (passthrough-unknown-operator condition))
    ((eq policy :recurse)     (recurse-unknown-operator condition))
    ((functionp policy)       (funcall policy condition))
    (t (error "~A: Invalid ON-UNKNOWN policy: ~A"
              (severity-color :error "Error")
              (lisp-to-string policy)))))

(defun signal-unknown-operator (interp op expr)
  "Signal an UNKNOWN-OPERATOR condition, offering USE-VALUE as a restart.
   Behavior when no outer handler intervenes is controlled by the interpreter's
   ON-UNKNOWN slot."
  (restart-case
      (let ((condition (make-condition 'unknown-operator
                                       :operator op
                                       :expression expr
                                       :interpreter interp)))
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

;; --- Convenience: one-shot handlers without mutating the interpreter ---

(defmacro with-passthrough-unknown (() &body body)
  `(handler-bind ((unknown-operator #'passthrough-unknown-operator))
     ,@body))

(defmacro with-recurse-unknown (() &body body)
  `(handler-bind ((unknown-operator #'recurse-unknown-operator))
     ,@body))


;; --- Splice values ---
;;
;; A SPLICE is a wrapper produced by a handler that wants to expand a single
;; AST node into a sequence of nodes. Whether the splice is *legal* depends
;; on the call site, not the rule:
;;
;;   - Splice context: the caller invoked LOWER with :splice T (or used
;;     RECURSE-SPLICE inside a handler body). The result is unwrapped into
;;     a list of nodes; a non-splice return is wrapped into a one-element
;;     list. RECURSE-SPLICE always returns a list.
;;
;;   - Expression context (the default): a splice return signals
;;     SPLICE-IN-EXPRESSION-CONTEXT. The single-value contract is enforced.
;;
;; Splice is purposely a struct (not a list-shaped tag like (:splice ...))
;; so it can never be confused with a real AST node.

(defstruct splice
  "Wrapper around a list of AST nodes that a handler wants to splice into
   the surrounding sequence at a splice-context call site."
  (nodes nil :type list))

(defun splice (&rest nodes)
  "Convenience constructor: (splice n1 n2 ...) builds a splice of NODES.
   Use (splice) for the empty splice (drops the node entirely)."
  (make-splice :nodes nodes))

(define-condition splice-in-expression-context (error)
  ((operator   :initarg :operator   :reader splice-in-expression-context-operator)
   (expression :initarg :expression :reader splice-in-expression-context-expression)
   (node-count :initarg :node-count :reader splice-in-expression-context-node-count))
  (:report (lambda (c stream)
             (format stream "~A: " (severity-color :error "Error"))
             (format stream
                     "Operator ~A returned a splice of ~A node~:P, but it was ~
                      lowered in expression context (which expects a single ~
                      value).~@
                      Expression: ~A~@
                      ~A To splice here, the caller must use ~A or ~
                      ~A with :SPLICE T, and the surrounding form must be ~
                      able to accept a sequence."
                     (lisp-to-string (splice-in-expression-context-operator c))
                     (bold (princ-to-string
                            (splice-in-expression-context-node-count c)))
                     (lisp-to-string (splice-in-expression-context-expression c))
                     (severity-color :hint "hint:")
                     (bold "RECURSE-SPLICE")
                     (bold "LOWER"))
             (let ((loc (source-loc-or-ancestor
                         (splice-in-expression-context-expression c))))
               (when loc
                 (format stream "~%  at:~%")
                 (print-source-context loc :stream stream))))))

;; --- Fresh name generation ---
;;
;; FRESH-NAME mints a unique symbol that hasn't been seen before and won't
;; collide with anything subsequently. This is the primitive a rule reaches
;; for when its expansion needs to introduce a binding -- e.g. a :swap rule
;; that lowers to (:let ((tmp a)) (:set a b) (:set b tmp)). Using a literal
;; TMP would capture the user's variable if they wrote (:swap tmp other);
;; FRESH-NAME prevents that by construction.
;;
;; The returned symbol is *uninterned* (made via MAKE-SYMBOL), matching the
;; convention CL's GENSYM uses. It still compares EQ to itself and prints
;; with a #: prefix, but doesn't pollute any package's symbol table.
;;
;; FRESH-NAME is also bound inside DEF-OP bodies as a local function (next
;; to RECURSE / RECURSE-SPLICE), so handlers can call (fresh-name) or
;; (fresh-name "tmp") directly. Calling it at top-level for ad-hoc AST
;; construction works too.

(defvar *fresh-counter* 0
  "Monotonic counter used by FRESH-NAME. Process-wide; thread-safety is
   provided by INCF being atomic on most implementations -- if you need
   stronger guarantees, wrap calls in your own lock.")

(defun fresh-name (&optional (prefix "g"))
  "Return a freshly minted, uninterned symbol. PREFIX is coerced to a string
   and used as the symbol's name root, suffixed with a unique counter
   value: (fresh-name)        => #:G1
   (fresh-name 'tmp)          => #:TMP2
   (fresh-name \"loop-end\")  => #:loop-end3
   The returned symbol is EQ-comparable to itself and to other references
   to the same call, but distinct from every other fresh symbol."
  (make-symbol (format nil "~A~D"
                       (string prefix)
                       (incf *fresh-counter*))))

;; --- Interpreter ---
;;
;; An interpreter is effectively a runtime macro system, which dispatches to a
;; collection of handlers defined on keywords. When the #'lower function is
;; called on an AST represented by cons cells, it will match the head of the
;; list to a handler, which will then return a new transformed AST. The handler
;; is allowed to recursively expand its body, so we have an outside-in
;; expansion mirroring Common Lisp macros.
;;
;; The interpreter/lowering function also provide the following functionality:
;;  - Splicing:  Setting the splice parameter in the lowering function ensures
;;               that the result is accumulated into a list of ASTs, which can
;;               then be used in the calling handler function. Otherwise, it is
;;               assumed that a lowering transformation only ever returns a
;;               single AST node.
;;  - Tracing:   Every call to the #'lower function can be optionally traced
;;               and accumulated into a collection of records for debugging
;;               purposes.
;;  - Overrides: Each lowering function is allowed to install local handlers
;;               that override interpreter-wide handlers within its local
;;               scope, similarly to Common Lisp's macrolet.
;;  - Gensym:    Fresh symbols can be generated within lowering calls by calling
;;               fresh-name. This is useful for transformations that produce
;;               intermediate variables/terms in the AST.
;;  - Source locations: When a handler returns a freshly-constructed cons,
;;               LOWER automatically attributes that cons to the input
;;               expression's source location (if it has one). Splice nodes
;;               each get the same attribution. The propagation uses
;;               INHERIT-LOC's first-wins semantics, so handler-set locs
;;               (via WITH-LOC / INHERIT-LOC) are never overwritten -- they
;;               just override the auto-propagated default. After many
;;               passes, derived nodes still point at the original source
;;               line that produced them.

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
                                to fall through to :error behavior.")
   (propagate-source-locations
    :initarg :propagate-source-locations
    :initform t
    :accessor propagate-source-locations
    :documentation
    "When true (the default), LOWER auto-attributes handler outputs to
     the input expression's source location. Set to NIL to disable
     auto-propagation (locs set explicitly by handlers still apply).")
   (readable-name
    :initarg :readable-name
    :initform "GENERIC INTERPRETER"
    :accessor readable-name
    :documentation
    "The name of the interpreter to print out in error message.")))

; Convenience function
(defun make-interpreter (&key (on-unknown :error)
                              (propagate-source-locations t)
                              (readable-name "GENERIC INTERPRETER")
                              )
  (make-instance 'interpreter
                 :on-unknown on-unknown
                 :propagate-source-locations propagate-source-locations
                 :readable-name readable-name))

(defgeneric lower (interpreter expression &key splice)
  (:documentation "Interprets the EXPRESSION using the rules defined in INTERPRETER.

   When SPLICE is NIL (the default), the call is in *expression context*:
   LOWER returns a single value. If a handler returns a SPLICE wrapper, a
   SPLICE-IN-EXPRESSION-CONTEXT error is signalled.

   When SPLICE is T, the call is in *splice context*: LOWER always returns
   a list of nodes. A handler that returns a SPLICE wrapper is unwrapped
   into its NODES list (possibly empty); a handler that returns a single
   value is wrapped into a one-element list.

   Source-loc propagation: after the handler returns, every cons in the
   result that doesn't already have a source-loc inherits one from EXPR.
   This is governed by the interpreter's PROPAGATE-SOURCE-LOCATIONS slot."))

(defmethod lower ((interp interpreter) (expr cons) &key splice)
  "Interprets a list by looking up the operator keyword in the interpreter's table.
   If no handler is found, signals UNKNOWN-OPERATOR. A USE-VALUE restart is
   available to substitute a replacement value for the sub-expression.
   Overrides installed via WITH-OVERRIDES take precedence over the
   interpreter's own handler table; see FIND-HANDLER."
  (let ((op (first expr)))
    (cond
      ((not (keywordp op))
       (error "~A: Interpreter expects keyword operators, got ~A"
              (severity-color :error "Error")
              (lisp-to-string op)))
      (t
       (call-with-trace-frame
        interp expr
        (lambda ()
          (let* ((handler (find-handler interp op))
                 (result  (if handler
                              (funcall handler expr)
                              (signal-unknown-operator interp op expr)))
                 (coerced (coerce-result result splice op expr)))
            (when (propagate-source-locations interp)
              (propagate-locs coerced expr splice))
            (values coerced (and handler t)))))))))

(defmethod lower ((interp interpreter) (expr t) &key splice)
  "Atoms (numbers, symbols, strings, ...) lower to themselves. They can
  also pass through a splice context as a one-element list -- a useful
  degenerate case (e.g. mapcan #'recurse-splice over a body containing
  both literals and statement-producing forms)."
  (call-with-trace-frame
   interp expr
   (lambda ()
     (values (coerce-result expr splice nil expr) nil))))

(defun coerce-result (result splice-context-p op expr)
  "Convert a handler's RESULT to the shape demanded by the call site.
   When SPLICE-CONTEXT-P is true, return a list (unwrap a SPLICE, wrap a
   non-splice). When false, return a single value (and error if RESULT is
   a SPLICE)."
  (cond
    (splice-context-p
     (if (splice-p result)
         (splice-nodes result)
         (list result)))
    ((splice-p result)
     (error 'splice-in-expression-context
            :operator op
            :expression expr
            :node-count (length (splice-nodes result))))
    (t result)))

(defun propagate-locs (coerced source splice-context-p)
  "Attribute SOURCE's source-loc to each top-level cons in COERCED that
   doesn't already have one. In splice context, COERCED is a list of
   nodes and each gets attribution. In expression context, COERCED is
   a single value (cons or atom) and only that value gets attribution.
   Uses INHERIT-LOC's first-wins semantics: existing attributions are
   preserved."
  (cond
    (splice-context-p
     (dolist (node coerced)
       (inherit-loc node source)))
    (t
     (inherit-loc coerced source))))

;; --- Tracing ---
;;
;; A trace is a tree of TRACE-ENTRY records, one per LOWER call. Each
;; entry records the input expression, the operator, whether a handler
;; fired, the final coerced output, and a list of children formed by
;; recursive LOWER calls made while computing the result.
;;
;; Trace entries also record the input's source-loc at frame-creation
;; time, so even if downstream transformations rebuild nodes, the trace
;; preserves the original-source attribution for display.
;;
;; Tracing is engaged via WITH-TRACE, which binds *TRACE-STACK* to a
;; one-element list whose head is a mutable accumulator for top-level
;; entries. When LOWER runs under a non-NIL *TRACE-STACK*, it pushes a
;; fresh accumulator while it processes its children, then collects the
;; finished node onto the parent accumulator. When *TRACE-STACK* is NIL
;; (the default), tracing is off and LOWER runs at zero overhead beyond
;; a single specvar read.

(defstruct trace-entry
  "A node in a lower-trace tree.

   INPUT       : the expression LOWER was called with (cons or atom)
   OPERATOR    : keyword if the input was a cons whose head is one;
                 otherwise NIL
   HANDLER-P   : T if a handler ran; NIL if the input was an atom or
                 an unknown operator with no handler
   OUTPUT      : the coerced result returned to LOWER's caller
   SOURCE-LOC  : the source-loc snapshot for INPUT at frame creation
                 time, or NIL if unknown
   CHILDREN    : list of TRACE-ENTRY nodes from recursive LOWER calls"
  input
  operator
  handler-p
  output
  source-loc
  (children '() :type list))

(defvar *trace-stack* nil
  "When tracing, a list of mutable accumulators (each a CONS whose CAR
   holds a list-being-built). LOWER pushes/pops on this stack so each
   call's recursive lowerings collect into a fresh sub-list. When NIL,
   tracing is off and LOWER does not allocate trace entries.")

(defun call-with-trace-frame (interp expr thunk)
  "Wrap THUNK with trace bookkeeping for a single LOWER call. THUNK is
   responsible for actually computing and returning the lowered result;
   this function records what happened around it."
  (declare (ignore interp))
  (if (null *trace-stack*)
      (funcall thunk)
      (let* ((children-cell (list nil))
             (op  (and (consp expr) (keywordp (first expr)) (first expr)))
             (loc (source-loc-or-ancestor expr)))
        (let ((*trace-stack* (cons children-cell *trace-stack*)))
          (multiple-value-bind (result handler-p)
              (funcall thunk)
            (let ((entry (make-trace-entry
                          :input expr
                          :operator op
                          :handler-p handler-p
                          ;; Copy-list the result if it's a list, so that
                          ;; downstream destructive operations (e.g. mapcan
                          ;; in a parent's splice handler) don't mutate
                          ;; the snapshot we recorded.
                          :output (if (listp result) (copy-list result) result)
                          :source-loc loc
                          :children (nreverse (car children-cell)))))
              ;; Push our entry onto our parent's accumulator.
              (push entry (car (second *trace-stack*)))
              result))))))

(defmacro with-trace (&body body)
  "Run BODY with tracing enabled. Returns (values BODY-VALUE TRACE),
   where TRACE is a list of TRACE-ENTRY trees -- one per top-level
   LOWER call performed in BODY."
  (let ((root (gensym "ROOT"))
        (val  (gensym "VAL")))
    `(let* ((,root (list nil))
            (*trace-stack* (list ,root))
            (,val (progn ,@body)))
       (values ,val (nreverse (car ,root))))))

(defun print-trace (trace &key (stream *standard-output*) (indent 0)
                               (show-locations t))
  "Pretty-print TRACE (a list of TRACE-ENTRY) to STREAM. Useful for
   eyeballing what fired during a lowering. Each line shows operator,
   input, output, and (when SHOW-LOCATIONS is true and the entry has
   one) the original source location of the input. Nested calls are
   indented."
  (dolist (entry trace)
    (let* ((op (trace-entry-operator entry))
           (op-str (cond
                     ((null op) (gray "<atom>"))
                     ((trace-entry-handler-p entry) (blue (princ-to-string op)
                                                          :bright t))
                     ;; No handler fired: dim the operator and tag it.
                     (t (format nil "~A~A"
                                (yellow (princ-to-string op))
                                (yellow "*"))))))
      (format stream "~&~v@T~A ~A ~A ~A~@[ ~A~]~%"
              indent
              op-str
              (lisp-to-string (trace-entry-input entry))
              (gray "=>")
              (lisp-to-string (trace-entry-output entry))
              (and show-locations
                   (trace-entry-source-loc entry)
                   (gray
                    (format nil "[at ~A]"
                            (format-source-loc
                             (trace-entry-source-loc entry)))))))
    (print-trace (trace-entry-children entry)
                 :stream stream
                 :indent (+ indent 2)
                 :show-locations show-locations)))

;; --- def-op ---
;;
;; (defmacro def-op (interp clause &body body) ...)
;;
;; def-op is a convenience macro that makes it easy to write pattern-matching
;; handlers for interpreters. You can define arbitrarily-nested patterns with
;; keyword arguments, optional arguments, and &rest arguments for a variable
;; number of arguments.
;;
;; The body of the def-op should evaluate to a list or an atom that represents
;; the AST to be returned after performing a transformation. Multiple ASTs
;; can be returned using the splice function, but only if requested by the
;; calling function. Otherwise, an error will be thrown.
;;
;; The body of the def-op can contain completely arbitrary code. The functions
;; recurse, recurse-splice, expr, and inherit-from are also locally defined:
;;   recurse        -- (recurse x) lowers x with the current interpreter
;;   recurse-splice -- (recurse-splice x) lowers x in splice context
;;   expr           -- (expr) returns the original input expression
;;   inherit-from   -- (inherit-from target source) tags TARGET with
;;                     SOURCE's source-loc; useful when a handler wants to
;;                     attribute a derived node to a specific sub-expression
;;                     rather than letting auto-propagation use the whole
;;                     input. First-wins semantics: a node only inherits if
;;                     it doesn't already have a loc, so existing
;;                     attributions are preserved.
;;
;; The lower function can be called with different interpreters to enable
;; mutual recursion if desired. Local handlers can also be registered, which
;; will dynamically override any set of handlers within the context of a
;; recursive call to lower, in a similar vein to macrolet.
;;
;; See the examples below for ways of defining interpreters using def-op.
;;
;; CLAUSE is (KEYWORD . LAMBDA-LIST). The lambda list now supports nested
;; patterns: any param/pattern position can be a sub-lambda-list with its
;; own &optional / &rest / &key. So all of the following are valid:
;;
;;   (def-op interp (:add a b)               ...)
;;   (def-op interp (:head (a b) c)          ...)   ; a, b destructure first arg
;;   (def-op interp (:head (a &optional b) c) ...)  ; with optional inside
;;   (def-op interp (:head &rest (x y &rest tail)) ...)
;;
;; &key vars must remain plain symbols (we need to derive the keyword name).
;;
;; Inside BODY, every leaf variable in the lambda list is in scope as a
;; normal lexical binding, and RECURSE is bound to a one-argument local
;; function that lowers a sub-expression with the *current* interpreter.
;; LOWER is unshadowed for cross-interpreter dispatch.
;;
;; TODO: add ignorable declarations for when variables aren't used

(defmacro def-op (interp clause &body body)
  (destructuring-bind (keyword &rest lambda-list) clause
    (unless (keywordp keyword)
      (error "def-op: clause head must be a keyword, got ~S" keyword))
    `(setf (gethash ,keyword (handlers ,interp))
           ,(make-handler-lambda interp clause lambda-list body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-handler-lambda (interp clause lambda-list body)
    "Return a form that evaluates to a handler lambda for CLAUSE on INTERP.
     Used by DEF-OP and WITH-OVERRIDES to share the lambda-list/handler
     emission logic. INTERP is a form (not necessarily a symbol) that
     evaluates to the target interpreter; CLAUSE is the source-level
     pattern used in error messages."
    (let* ((parsed (parse-lambda-list lambda-list))
           (all-vars (flatten-vars parsed))
           (expr (gensym "EXPR")))
      `(lambda (,expr)
         (destructuring-bind ,all-vars
             (match-lambda-list
              ,(emit-parsed-ll parsed)
              (rest ,expr)
              ,(first clause)
              ,expr
              ',clause)
           ;; Pre-declare all lambda-list variables ignorable so handlers
           ;; don't need their own (declare (ignore ...)) for unused params.
           ;; This must sit directly in the destructuring-bind body (not
           ;; inside the flet below) so SBCL correctly associates the
           ;; declaration with the bindings it introduces.
           ,@(when all-vars `((declare (ignorable ,@all-vars))))
           (flet ((recurse (x) (lower ,interp x))
                  (recurse-splice (x) (lower ,interp x :splice t))
                  (expr () ,expr)
                  (inherit-from (target source) (inherit-loc target source)))
             (declare (ignorable #'recurse #'recurse-splice
                                 #'expr #'inherit-from))
             ,@body))))))

;; --- Local rule overrides ---
;;
;; *OVERRIDES* is an FSet map from interpreter to (FSet map from operator
;; keyword to handler function). WITH-OVERRIDES rebinds *OVERRIDES* to an
;; extended version for the dynamic extent of its body.
;;
;; Because FSet maps are persistent, "extend" is a pure function: the
;; outer binding is unaffected by the inner one, and nested overrides
;; just stack via successive LET bindings.
;;
;; Override clauses use the same syntax as DEF-OP, so nested patterns,
;; &optional/&rest/&key, splice, recurse, and fresh-name all work the
;; same inside an override.

(defvar *overrides* (fset:empty-map)
  "FSet map from interpreter -> (FSet map from operator keyword -> handler).
   Consulted by FIND-HANDLER before the interpreter's own handler table.")

(defun find-handler (interp op)
  "Look up a handler for OP on INTERP. Consults *OVERRIDES* first; on a
   miss, falls through to the interpreter's hash table. Returns the
   handler function or NIL."
  (let ((interp-overrides (fset:lookup *overrides* interp)))
    (or (and interp-overrides (fset:lookup interp-overrides op))
        (gethash op (handlers interp)))))

(defmacro with-overrides ((interp &rest clauses) &body body)
  "Install handler overrides on INTERP for the dynamic extent of BODY.
   CLAUSES is a list of DEF-OP-shaped clauses: each is
     ((KEYWORD . LAMBDA-LIST) BODY-FORM*)
   The override binding is restored on any exit (normal or non-local)
   via standard CL special-variable semantics."
  (let ((interp-var (gensym "INTERP")))
    ;; Each clause becomes (keyword handler-lambda) at runtime, the
    ;; pairs of which extend the interpreter's override map.
    (let ((pairs
            (loop for clause-with-body in clauses
                  collect (destructuring-bind ((keyword &rest ll) &rest cb)
                              clause-with-body
                            (unless (keywordp keyword)
                              (error "with-overrides: clause head must be ~
                                      a keyword, got ~S" keyword))
                            `(list ,keyword
                                   ,(make-handler-lambda
                                     interp-var
                                     (cons keyword ll)
                                     ll
                                     cb))))))
      `(let* ((,interp-var ,interp)
              (*overrides*
                (fset:with *overrides*
                           ,interp-var
                           (reduce (lambda (m pair)
                                     (fset:with m (first pair) (second pair)))
                                   (list ,@pairs)
                                   :initial-value
                                   (or (fset:lookup *overrides* ,interp-var)
                                       (fset:empty-map))))))
         ,@body))))


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
