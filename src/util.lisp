(in-package #:tagless-compiler)

;; --- def-op lambda list parser ---
;;
;; A def-op lambda list looks like:
;;   (positional*
;;    [&optional opt-spec*]
;;    [&rest sym]
;;    [&key key-spec*])
;; where each opt-spec or key-spec is either SYM or (SYM DEFAULT).
;;
;; This is a strict subset of CL's lambda lists. Matching happens at runtime
;; in MATCH-LAMBDA-LIST so DEF-OP itself stays small: the macro only parses
;; the lambda list (at compile time, for early error reporting), turns the
;; default forms into thunks, and emits a thin handler that calls the matcher
;; and runs BODY with the resulting bindings installed via PROGV.

;; The parser is wrapped in EVAL-WHEN so its definition is in effect during
;; compilation, not just at load time -- DEF-OP calls it at macroexpansion
;; time, which means file compilation needs the function to already be
;; defined in the compilation environment.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-lambda-list (lambda-list)
    "Parse LAMBDA-LIST into (values POSITIONALS OPTIONALS REST-VAR KEY-SPECS).
   POSITIONALS is a list of symbols.
   OPTIONALS is a list of (VAR DEFAULT-FORM).
   REST-VAR is a symbol or NIL.
   KEY-SPECS is a list of (KEYWORD VAR DEFAULT-FORM).
   DEFAULT-FORM is a raw form -- the macro turns it into a thunk before
   passing it to MATCH-LAMBDA-LIST."
    (let ((positionals '())
          (optionals   '())
          (rest-var    nil)
          (key-specs   '())
          (state       :positional))
      (labels ((normalize-with-default (spec what)
                 (cond ((symbolp spec)
                        (values spec nil))
                       ((and (consp spec)
                             (symbolp (car spec))
                             (or (null (cdr spec)) (null (cddr spec))))
                        (values (car spec) (cadr spec)))
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
                         (setf rest-var (pop lambda-list)))
              (&key      (advance-state :key      '(:positional :optional :rest)))
              (t (ecase state
                   (:positional
                    (unless (and (symbolp item) (not (keywordp item)))
                      (error "def-op: positional parameter must be a non-keyword symbol, got ~S" item))
                    (push item positionals))
                   (:optional
                    (multiple-value-bind (var default)
                        (normalize-with-default item "&optional")
                      (push (list var default) optionals)))
                   (:rest
                    (error "def-op: extra forms after &rest variable: ~S" item))
                   (:key
                    (multiple-value-bind (var default)
                        (normalize-with-default item "&key")
                      (push (list (intern (symbol-name var) :keyword) var default)
                            key-specs)))))))))
      (values (nreverse positionals)
              (nreverse optionals)
              rest-var
              (nreverse key-specs)))))

;; --- Runtime matcher ---
;;
;; MATCH-LAMBDA-LIST consumes ARGS (the call site's arguments, i.e. (REST
;; EXPR)) and returns a flat list of values, one per variable in the lambda
;; list, in lambda-list order: positionals, then &optional, then &rest (if
;; any), then &key vars. The macro then DESTRUCTURING-BINDs that list to
;; lexical names in the body's scope.
;;
;; OPTIONALS and KEY-SPECS carry default *thunks* (zero-argument functions)
;; rather than raw forms, so the matcher doesn't need EVAL: it just FUNCALLs
;; a thunk when a default is needed.
;;
;; Any structural mismatch -- missing required arg, trailing junk, malformed
;; or unknown keyword -- signals MALFORMED-OPERATOR-ARGS with the offending
;; operator, original expression, and the source-level pattern. This is an
;; advantage over destructuring-bind since it only returns a (not very
;; informative) error.

(defun match-lambda-list (n-required optionals rest-var key-specs args
                          operator expression pattern)
  "Match ARGS against a parsed lambda list. Return a flat list of values,
   one per variable in the lambda list, in lambda-list order. Signal
   MALFORMED-OPERATOR-ARGS on shape mismatch.

   N-REQUIRED : number of required positional parameters
   OPTIONALS  : list of (symbol thunk) -- symbol is unused at runtime
   REST-VAR   : symbol or NIL -- only the truthiness matters at runtime
   KEY-SPECS  : list of (keyword symbol thunk) -- symbol is unused at runtime"
  (flet ((fail ()
           (error 'malformed-operator-args
                  :operator operator
                  :expression expression
                  :pattern pattern)))
    (let ((values    '())
          (remaining args))
      ;; 1. Required positionals.
      (loop repeat n-required do
        (unless remaining (fail))
        (push (pop remaining) values))
      ;; 2. Optionals.
      (dolist (spec optionals)
        (let ((thunk (second spec)))
          (push (if remaining
                    (pop remaining)
                    (and thunk (funcall thunk)))
                values)))
      ;; 3. &rest captures everything still in REMAINING. With &key, the same
      ;;    tail is *also* parsed below; this matches CL's &rest+&key semantics.
      (when rest-var
        (push remaining values))
      ;; 4. &key.
      (cond
        (key-specs
         (let ((seen (make-hash-table :test #'eq))
               (allowed (mapcar #'first key-specs)))
           (loop while remaining do
             (let ((k (pop remaining)))
               (unless (and (keywordp k) (member k allowed)) (fail))
               (unless remaining (fail))
               (setf (gethash k seen) (pop remaining))))
           (dolist (spec key-specs)
             (let ((kw (first spec))
                   (thunk (third spec)))
               (multiple-value-bind (val present) (gethash kw seen)
                 (push (if present val (and thunk (funcall thunk)))
                       values))))))
        ((and (not rest-var) remaining)
         (fail)))
      (nreverse values))))

