(in-package #:tagless-compiler)

;; --- def-op lambda list parser ---
;;
;; A def-op lambda list looks like:
;;   ([&whole var]
;;    param*
;;    [&optional opt-spec*]
;;    [&rest pattern]
;;    [&key key-spec*])
;; where:
;;   param    is either a symbol or a nested lambda list (cons)
;;   opt-spec is PATTERN | (PATTERN DEFAULT)
;;   key-spec is PATTERN | (PATTERN DEFAULT)        ; the keyword is derived
;;                                                    from PATTERN -- see below
;;   pattern  (recursive position) is a symbol or a nested lambda list
;;
;; Nested lambda lists have full parity with top-level: any param/pattern
;; position can itself be a list with its own &whole / &optional / &rest /
;; &key. This means (def-op interp (:head (a b) c) ...) destructures the
;; first argument, and (def-op interp (:head (&whole pair a b) c) ...)
;; binds PAIR to the original (a b) cons -- preserving its source-loc --
;; while still destructuring it into A and B.
;;
;; --- &whole and source-loc propagation ---
;;
;; When the matcher destructures a sub-pattern like (type name) from an
;; input cons, the *value* it pulled apart is a cons that has a source-loc
;; in *SOURCE-LOCATIONS*. The leaves type and name are atoms and can't
;; carry locs, so a handler that wants to reuse the (type name) sub-cons
;; with its loc intact can use &whole to get a binding for the original:
;;
;;   (def-op interp (:declare (&whole tn type name) value)
;;     (list :declare tn (recurse value)))    ; tn keeps its loc
;;
;; Without &whole, the handler would have to rebuild (list type name) and
;; then call inherit-from manually. With it, the matcher hands back the
;; original cons and propagation is automatic.
;;
;; Matching happens at runtime in MATCH-LAMBDA-LIST so DEF-OP itself stays
;; small: the macro parses the lambda list (at compile time, for early
;; error reporting), turns the default forms into thunks, and emits a
;; thin handler that calls the matcher and runs BODY with the resulting
;; bindings installed by DESTRUCTURING-BIND on a flat var list.
;;
;; --- Note on &key with nested patterns ---
;;
;; A &key spec like (:head &key (count 0)) derives its keyword from the
;; symbol -- :COUNT here. When the variable position is itself a nested
;; pattern, there's no symbol to derive a keyword from, so &key parameters
;; must remain symbols. The parser raises a clear error if asked to nest
;; under &key. (&whole, &optional, and &rest all nest fine.)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct parsed-ll
    "A parsed lambda list, used both at top level and recursively for
     nested patterns. WHOLE-VAR is a symbol or NIL; if non-nil, the
     entire input list at this level is bound to that name (in addition
     to any leaf bindings from the destructured pattern). POSITIONALS
     is a list whose elements are either symbols (leaf) or PARSED-LL
     structs (nested). OPTIONALS is a list of (PARAM DEFAULT-FORM) where
     PARAM is symbol-or-PARSED-LL. REST-VAR is a symbol, a PARSED-LL,
     or NIL. KEY-SPECS is a list of (KEYWORD SYMBOL DEFAULT-FORM) --
     key vars are restricted to symbols, see file header."
    whole-var
    positionals
    optionals
    rest-var
    key-specs)

  (defun parse-pattern (pattern context)
    "Parse PATTERN, which appears in a variable position. CONTEXT is a
     short string used in error messages (e.g. \"positional\", \"&optional\",
     \"&rest\"). Returns a symbol or a PARSED-LL struct."
    (cond ((and (symbolp pattern) (not (keywordp pattern)))
           pattern)
          ((consp pattern)
           (parse-lambda-list pattern))
          (t
           (error "def-op: ~A parameter must be a non-keyword symbol or a ~
                   nested lambda list, got ~S" context pattern))))

  (defun parse-lambda-list (lambda-list)
    "Parse LAMBDA-LIST into a PARSED-LL struct.

     Each parameter position accepts either a symbol or a nested lambda
     list (which is recursively parsed). DEFAULT-FORM is left as a raw
     form -- the macro turns it into a thunk before passing the parsed
     structure to MATCH-LAMBDA-LIST.

     &whole, if present, must be the first element and is followed by
     a non-keyword symbol that will be bound to the entire input value
     at this nesting level."
    (unless (listp lambda-list)
      (error "def-op: lambda list must be a list, got ~S" lambda-list))
    (let ((whole-var   nil)
          (positionals '())
          (optionals   '())
          (rest-var    nil)
          (key-specs   '())
          (state       :positional))
      ;; &whole must be the very first element if present. We peel it off
      ;; here, before the main loop, so the state machine doesn't have to
      ;; carry an awkward :whole-allowed flag.
      (when (and lambda-list (eq (first lambda-list) '&whole))
        (pop lambda-list)
        (unless lambda-list
          (error "def-op: &whole must be followed by a variable"))
        (let ((var (pop lambda-list)))
          (unless (and (symbolp var) (not (keywordp var)))
            (error "def-op: &whole variable must be a non-keyword symbol, ~
                    got ~S" var))
          (setf whole-var var)))
      (labels ((split-spec (spec what)
                 ;; Return (values PARAM DEFAULT-FORM) for an &optional or &key
                 ;; spec. PARAM is the raw form -- caller decides whether to
                 ;; recurse (via PARSE-PATTERN) or require a symbol.
                 (cond ((or (symbolp spec) (consp spec))
                        ;; SYM or (SYM) or (SYM DEFAULT) -- but a nested
                        ;; pattern like (a b) is also a cons, so be careful
                        ;; about which case we're in. We disambiguate by
                        ;; looking at the spec's *length* and whether the
                        ;; head is itself a cons:
                        ;;   - (sym)            -> param=sym, default=nil
                        ;;   - (sym default)    -> param=sym, default=default
                        ;;   - (pattern)        -> param=pattern, default=nil
                        ;;   - (pattern default)-> param=pattern, default=default
                        ;; The rule that resolves the ambiguity: an opt/key
                        ;; spec is *always* of the form (PARAM) or
                        ;; (PARAM DEFAULT), so we check arity. A nested
                        ;; pattern that *looks like* a 2-element list will
                        ;; be misread as (param default) -- so we require
                        ;; users to wrap nested-pattern opt/key specs as
                        ;; ((nested-pattern)) or ((nested-pattern) default).
                        ;; Plain symbols still work bare: (sym), sym,
                        ;; (sym default).
                        (cond
                          ((symbolp spec)
                           (values spec nil))
                          ((null (cdr spec))
                           (values (car spec) nil))
                          ((null (cddr spec))
                           (values (car spec) (cadr spec)))
                          (t (error "def-op: bad ~A spec ~S" what spec))))
                       (t (error "def-op: bad ~A spec ~S" what spec))))
               (advance-state (new old-allowed)
                 (unless (member state old-allowed)
                   (error "def-op: ~S after ~S in ~S" new state lambda-list))
                 (setf state new)))
        (loop while lambda-list do
          (let ((item (pop lambda-list)))
            (case item
              (&whole    (error "def-op: &whole must be the first element ~
                                 of a lambda list"))
              (&optional (advance-state :optional '(:positional)))
              (&rest     (advance-state :rest     '(:positional :optional))
                         (unless lambda-list
                           (error "def-op: &rest must be followed by a variable"))
                         (setf rest-var (parse-pattern (pop lambda-list)
                                                       "&rest")))
              (&key      (advance-state :key      '(:positional :optional :rest)))
              (t (ecase state
                   (:positional
                    (push (parse-pattern item "positional") positionals))
                   (:optional
                    (multiple-value-bind (raw default) (split-spec item "&optional")
                      (push (list (parse-pattern raw "&optional") default)
                            optionals)))
                   (:rest
                    (error "def-op: extra forms after &rest variable: ~S" item))
                   (:key
                    (multiple-value-bind (raw default) (split-spec item "&key")
                      ;; &key vars must be plain symbols -- we need a name to
                      ;; derive the keyword from.
                      (unless (and (symbolp raw) (not (keywordp raw)))
                        (error "def-op: &key parameter must be a non-keyword ~
                                symbol (cannot nest patterns under &key), ~
                                got ~S" raw))
                      (push (list (intern (symbol-name raw) :keyword) raw default)
                            key-specs)))))))))
      (make-parsed-ll
       :whole-var   whole-var
       :positionals (nreverse positionals)
       :optionals   (nreverse optionals)
       :rest-var    rest-var
       :key-specs   (nreverse key-specs))))

  (defun flatten-vars (parsed)
    "Walk PARSED (a PARSED-LL) and return the leaf variable symbols in
     match order: &whole (if any), then positionals (recursing into
     nested), then optionals (recursing), then rest (recursing), then
     keys."
    (let ((acc '()))
      (labels ((walk-param (p)
                 (cond ((null p))
                       ((symbolp p) (push p acc))
                       ((parsed-ll-p p) (walk-ll p))))
               (walk-ll (ll)
                 ;; &whole comes first to match the matcher's emission order.
                 (when (parsed-ll-whole-var ll)
                   (push (parsed-ll-whole-var ll) acc))
                 (dolist (p (parsed-ll-positionals ll)) (walk-param p))
                 (dolist (spec (parsed-ll-optionals ll))
                   (walk-param (first spec)))
                 (walk-param (parsed-ll-rest-var ll))
                 (dolist (spec (parsed-ll-key-specs ll))
                   (push (second spec) acc))))
        (walk-ll parsed)
        (nreverse acc)))))

;; --- Runtime matcher ---
;;
;; MATCH-LAMBDA-LIST consumes ARGS (the call site's arguments) and a parsed
;; lambda list, and returns a flat list of values, one per leaf variable in
;; the lambda list, in the same order FLATTEN-VARS produces. The macro then
;; DESTRUCTURING-BINDs that flat list to the flat variable list.
;;
;; OPTIONALS and KEY-SPECS in the runtime structure carry default *thunks*
;; (zero-argument functions) rather than raw forms, so the matcher doesn't
;; need EVAL: it just FUNCALLs a thunk when a default is needed.
;;
;; Any structural mismatch -- missing required arg, trailing junk, malformed
;; or unknown keyword, atom where a list pattern was expected -- signals
;; MALFORMED-OPERATOR-ARGS with the offending operator, the *original*
;; expression, and the *source-level* pattern (the user-written clause).
;; Recursive calls thread these through unchanged so nested mismatches
;; still point at the top-level def-op site.

(defun match-lambda-list (parsed args operator expression pattern)
  "Match ARGS against PARSED (a PARSED-LL). Return a flat list of values
   in flatten-vars order. Signal MALFORMED-OPERATOR-ARGS on any shape
   mismatch."
  (flet ((fail ()
           (error 'malformed-operator-args
                  :operator operator
                  :expression expression
                  :pattern pattern)))
    (labels ((match-one (param value)
               ;; Match a single value against a single parameter
               ;; (symbol or nested PARSED-LL). Returns a list of values
               ;; in flatten-vars order for that parameter's subtree.
               ;;
               ;; For a nested pattern, the sub-input VALUE is the
               ;; original cons that the caller is destructuring -- and
               ;; if the nested pattern has &whole, that var binds to
               ;; this exact cons, source-loc and all.
               (cond ((symbolp param)
                      (list value))
                     ((parsed-ll-p param)
                      (unless (listp value) (fail))
                      (match-ll param value))
                     (t (fail))))
             (match-ll (ll args)
               (let ((values    '())
                     (remaining args))
                 ;; 0. &whole captures the input list at this level. The
                 ;; binding holds the original cons (or NIL for an empty
                 ;; list), so any source-loc on it is preserved.
                 (when (parsed-ll-whole-var ll)
                   (push args values))
                 ;; 1. Required positionals.
                 (dolist (param (parsed-ll-positionals ll))
                   (unless remaining (fail))
                   (dolist (v (match-one param (pop remaining)))
                     (push v values)))
                 ;; 2. Optionals.
                 (dolist (spec (parsed-ll-optionals ll))
                   (let* ((param (first spec))
                          (thunk (second spec))
                          (value (if remaining
                                     (pop remaining)
                                     (and thunk (funcall thunk)))))
                     ;; If the optional was omitted *and* there's no default,
                     ;; we still need to populate bindings for any leaves the
                     ;; param introduces. For a symbol, that's one NIL; for
                     ;; a nested pattern with no default, defaulting to NIL
                     ;; is incoherent -- match-one would fail on most shapes.
                     ;; Treat "no default + nested pattern + missing" as
                     ;; binding all leaves to NIL.
                     (cond
                       ((and (not remaining) (not thunk) (parsed-ll-p param))
                        (dolist (_ (flatten-vars param))
                          (declare (ignore _))
                          (push nil values)))
                       (t
                        (dolist (v (match-one param value))
                          (push v values))))))
                 ;; 3. &rest captures everything still in REMAINING. With
                 ;; &key, the same tail is *also* parsed below; this matches
                 ;; CL's &rest+&key semantics.
                 (when (parsed-ll-rest-var ll)
                   (let ((rest-param (parsed-ll-rest-var ll)))
                     (cond ((symbolp rest-param)
                            (push remaining values))
                           ((parsed-ll-p rest-param)
                            (dolist (v (match-ll rest-param remaining))
                              (push v values))))))
                 ;; 4. &key.
                 (cond
                   ((parsed-ll-key-specs ll)
                    (let ((seen (make-hash-table :test #'eq))
                          (allowed (mapcar #'first (parsed-ll-key-specs ll))))
                      (loop while remaining do
                        (let ((k (pop remaining)))
                          (unless (and (keywordp k) (member k allowed)) (fail))
                          (unless remaining (fail))
                          (setf (gethash k seen) (pop remaining))))
                      (dolist (spec (parsed-ll-key-specs ll))
                        (let ((kw (first spec))
                              (thunk (third spec)))
                          (multiple-value-bind (val present) (gethash kw seen)
                            (push (if present val (and thunk (funcall thunk)))
                                  values))))))
                   ((and (not (parsed-ll-rest-var ll)) remaining)
                    (fail)))
                 (nreverse values))))
      (match-ll parsed args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-parsed-ll (parsed)
    "Emit a form that, when evaluated, reconstructs PARSED with default
     forms wrapped in zero-argument thunks. Symbols are emitted as quoted
     symbols; nested PARSED-LLs recurse."
    (labels ((emit-param (p)
               (cond ((null p) 'nil)
                     ((symbolp p) `',p)
                     ((parsed-ll-p p) (emit-parsed-ll p))))
             (thunk (form)
               (if form `(lambda () ,form) 'nil)))
      `(make-parsed-ll
        :whole-var   ',(parsed-ll-whole-var parsed)
        :positionals (list ,@(mapcar #'emit-param
                                     (parsed-ll-positionals parsed)))
        :optionals   (list ,@(loop for (param default)
                                   in (parsed-ll-optionals parsed)
                                   collect `(list ,(emit-param param)
                                                  ,(thunk default))))
        :rest-var    ,(emit-param (parsed-ll-rest-var parsed))
        :key-specs   (list ,@(loop for (kw var default)
                                   in (parsed-ll-key-specs parsed)
                                   collect `(list ,kw ',var
                                                  ,(thunk default))))))))


;; --- Pattern matching helpers for handler bodies ---
;;
;; DEF-OP installs a single pattern at the boundary between LOWER's
;; dispatch and the handler's body. That covers the common "destructure
;; my input once" case but not handlers that need to look at a
;; sub-expression and decide what to do based on its shape -- e.g. a
;; rule that lowers (:set lhs val) differently when LHS is a bare
;; variable vs. a (:field obj name) access.
;;
;; MATCH-PATTERN runs a single def-op-style pattern on an arbitrary
;; expression and binds the leaf variables for a body. MATCH-CASES is
;; the multi-clause form: it tries patterns in order, runs the first
;; matching body, and signals if no clause matches (unless a T fallback
;; is supplied).
;;
;; Both macros call INHERIT-LOC on their result so that any cons the
;; body returns gets the matched expression's source-loc -- the same
;; first-wins propagation LOWER does at the dispatch level. This means
;; you can use these freely in handler bodies without worrying about
;; locs falling off newly constructed sub-trees.
;;
;; Patterns are the same as DEF-OP's lambda lists, except they include
;; the operator keyword as the head (since MATCH-PATTERN matches a
;; whole expression, not just its arguments). So:
;;
;;   (match-pattern node (:set (&whole lhs lhs-name) val)
;;     (list :store lhs val))
;;
;; matches NODE against (:set (some-name) some-val), with LHS bound to
;; the original (some-name) cons and LHS-NAME, VAL bound to the leaves.
;; If NODE doesn't match, MALFORMED-OPERATOR-ARGS is signalled.

(defmacro match-pattern (expr (operator &rest lambda-list) &body body)
  "Match EXPR against (OPERATOR . LAMBDA-LIST) and run BODY with the
   leaf bindings. Signal MALFORMED-OPERATOR-ARGS on mismatch. The
   value of BODY (if it's a cons that doesn't already have a loc)
   inherits EXPR's source-loc."
  (unless (keywordp operator)
    (error "match-pattern: pattern head must be a keyword, got ~S" operator))
  (let* ((parsed   (parse-lambda-list lambda-list))
         (all-vars (flatten-vars parsed))
         (g-expr   (gensym "EXPR"))
         (g-result (gensym "RESULT")))
    `(let ((,g-expr ,expr))
       (unless (and (consp ,g-expr) (eq (first ,g-expr) ,operator))
         (error 'malformed-operator-args
                :operator ,operator
                :expression ,g-expr
                :pattern '(,operator ,@lambda-list)))
       (let ((,g-result
               (destructuring-bind ,all-vars
                   (match-lambda-list ,(emit-parsed-ll parsed)
                                      (rest ,g-expr)
                                      ,operator
                                      ,g-expr
                                      '(,operator ,@lambda-list))
                 ,@body)))
         (inherit-loc ,g-result ,g-expr)))))

(defmacro match-cases (expr &body clauses)
  "Try CLAUSES in order against EXPR and run the body of the first
   match. Each clause is one of:

     ((OPERATOR . LAMBDA-LIST) BODY*)        -- pattern clause
     (T BODY*)                               -- catch-all (must be last)

   If no clause matches, signal MALFORMED-OPERATOR-ARGS. The value of
   the matching body inherits EXPR's source-loc.

   Usage:
     (match-cases (recurse target)
       ((:var name)            (list :load name))
       ((:field obj name)      (list :load-field (recurse obj) name))
       (t                      (error \"unsupported lvalue: ~S\" target)))"
  (let ((g-expr   (gensym "EXPR"))
        (g-result (gensym "RESULT"))
        (block-name (gensym "MATCH-CASES")))
    (labels ((emit-clause (clause)
               (cond
                 ;; T fallback: no match attempt, just run the body.
                 ((and (consp clause) (eq (car clause) t))
                  `(return-from ,block-name
                     (progn ,@(rest clause))))
                 ;; Pattern clause.
                 ((and (consp clause) (consp (car clause)))
                  (destructuring-bind ((operator &rest lambda-list)
                                       &rest body)
                      clause
                    (unless (keywordp operator)
                      (error "match-cases: pattern head must be a keyword, ~
                              got ~S" operator))
                    (let* ((parsed   (parse-lambda-list lambda-list))
                           (all-vars (flatten-vars parsed)))
                      `(when (and (consp ,g-expr)
                                  (eq (first ,g-expr) ,operator))
                         ;; A successful EQ match commits to this clause.
                         ;; If the args don't destructure, we let the
                         ;; MALFORMED-OPERATOR-ARGS error propagate --
                         ;; falling through to a later clause would mask
                         ;; bugs in the user's pattern.
                         (return-from ,block-name
                           (destructuring-bind ,all-vars
                               (match-lambda-list
                                ,(emit-parsed-ll parsed)
                                (rest ,g-expr)
                                ,operator
                                ,g-expr
                                '(,operator ,@lambda-list))
                             ,@body)))))))
                 (t (error "match-cases: malformed clause ~S" clause)))))
      `(let* ((,g-expr ,expr)
              (,g-result
                (block ,block-name
                  ,@(mapcar #'emit-clause clauses)
                  (error 'malformed-operator-args
                         :operator (and (consp ,g-expr) (first ,g-expr))
                         :expression ,g-expr
                         :pattern '(match-cases
                                    ,@(loop for c in clauses
                                            when (and (consp c)
                                                      (consp (car c)))
                                            collect (car c)))))))
         (inherit-loc ,g-result ,g-expr))))
