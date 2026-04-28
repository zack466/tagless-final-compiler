(in-package #:tagless-compiler)

;; --- def-op lambda list parser ---
;;
;; A def-op lambda list looks like:
;;   (param*
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
;; position can itself be a list with its own &optional / &rest / &key.
;; This means (def-op interp (:head (a b) c) ...) destructures the first
;; argument, and (def-op interp (:head (a &optional (b 0)) c) ...) does
;; the obvious thing.
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
;; under &key. (&optional and &rest both nest fine.)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct parsed-ll
    "A parsed lambda list, used both at top level and recursively for
     nested patterns. POSITIONALS is a list whose elements are either
     symbols (leaf) or PARSED-LL structs (nested). OPTIONALS is a list
     of (PARAM DEFAULT-FORM) where PARAM is symbol-or-PARSED-LL.
     REST-VAR is a symbol, a PARSED-LL, or NIL. KEY-SPECS is a list of
     (KEYWORD SYMBOL DEFAULT-FORM) -- key vars are restricted to
     symbols, see file header."
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
     structure to MATCH-LAMBDA-LIST."
    (unless (listp lambda-list)
      (error "def-op: lambda list must be a list, got ~S" lambda-list))
    (let ((positionals '())
          (optionals   '())
          (rest-var    nil)
          (key-specs   '())
          (state       :positional))
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
       :positionals (nreverse positionals)
       :optionals   (nreverse optionals)
       :rest-var    rest-var
       :key-specs   (nreverse key-specs))))

  (defun flatten-vars (parsed)
    "Walk PARSED (a PARSED-LL) and return the leaf variable symbols in
     match order: positionals (recursing into nested), then optionals
     (recursing), then rest (recursing), then keys."
    (let ((acc '()))
      (labels ((walk-param (p)
                 (cond ((null p))
                       ((symbolp p) (push p acc))
                       ((parsed-ll-p p) (walk-ll p))))
               (walk-ll (ll)
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
               (cond ((symbolp param)
                      (list value))
                     ((parsed-ll-p param)
                      (unless (listp value) (fail))
                      (match-ll param value))
                     (t (fail))))
             (match-ll (ll args)
               (let ((values    '())
                     (remaining args))
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

