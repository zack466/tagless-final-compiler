(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

;;; ---------------------------------------------------------------------------
;;; Conditions and the try-match restart helper
;;; ---------------------------------------------------------------------------

(define-condition match-error (error)
  ((ast  :initarg :ast   :reader match-error-ast)
   (rule :initarg :rule  :reader match-error-rule))
  (:report (lambda (c stream)
             (format stream "~A: " (severity-color :error "Match Error"))
             (format stream
                     "AST node ~A does not match rule: ~A"
                     (lisp-to-string (match-error-ast c))
                     (lisp-to-string (match-error-rule c)))
             (let ((loc (source-loc-or-ancestor (match-error-ast c))))
               (when loc
                 (format stream "~%  at:~%")
                 (print-source-context loc :stream stream))))))

(defun signal-match-error (ast rule)
  (error (make-condition 'match-error :ast ast :rule rule)))

;; Tries to match. On failure, returns (values nil nil <ast-at-failure-point>).
(defmacro try-match (&body body)
  `(restart-case
       (handler-bind ((match-error
                        (lambda (c)
                          (let ((restart (find-restart 'use-value c)))
                            (invoke-restart restart (match-error-ast c))))))
         ,@body)
     (use-value (remaining-ast)
       (values nil nil remaining-ast))))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun get-rule (keyword rules)
  "Returns two values: the slot patterns for the rule, and a flag indicating
   whether the rule was found. An empty body is distinct from a missing rule —
   rules like (:break) have no slots but are valid."
  (let ((entry (assoc keyword rules :test #'eq)))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun literal-p (expr)
  "A literal is a number, boolean, or string."
  (or (numberp expr)
      (typep expr 'boolean)
      (stringp expr)))

(defun keyword-ref-p (rule)
  "True if RULE is a bare keyword referring to another grammar rule."
  (and (symbolp rule) (keywordp rule)))

(defparameter *combinators*
  '(keyword identifier symbol literal maybe option repeat0 dispatch)
  "Symbols that, when at the head of a list, mark the list as a combinator
   rather than an implicit sequence.")

(defun combinator-p (rule)
  "True if RULE is a combinator form — a list whose head is one of the
   recognized combinator symbols."
  (and (consp rule)
       (symbolp (car rule))
       (member (car rule) *combinators* :test #'eq)))

(defun abstract-rule-body (body)
  "If BODY is the body of an abstract rule (a one-element list containing a
   single (dispatch ...) form), return the dispatch's inner form. Otherwise
   return nil."
  (when (and (= (length body) 1)
             (consp (car body))
             (eq (caar body) 'dispatch))
    (cadar body)))

;;; ---------------------------------------------------------------------------
;;; The matcher
;;;
;;; Semantic model:
;;; - A keyword (e.g. :type) is a reference to a named rule. Matching it
;;;   consumes one wrapped sub-node (:type ...) and recurses into the named
;;;   rule's body.
;;; - A list whose head is a combinator symbol (option, keyword, identifier,
;;;   etc.) is dispatched as that combinator.
;;; - Any other list is implicitly a sequence each element is a slot pattern
;;;   matched in order against consecutive AST elements at the current level.
;;;
;;; All matchers return (values success matched remaining).
;;; ---------------------------------------------------------------------------
;;; The matcher provides the special combinators:
;;; - (keyword <KEYWORD>)               - matches exactly the keyword <KEYWORD>
;;; - (identifier)                      - matches any symbol that is not a keyword or nil
;;; - (symbol)                          - matches any symbol
;;; - (literal)                         - matches any literal
;;; - (maybe <RULE>)                    - matches <RULE> or doesn't match at all
;;; - (option <RULE1> <RULE2> ...)      - tries all of the rules in order
;;; - (repeat0 <RULE>)                  - matches <RULE> exactly 0 or more times
;;; - (dispatch <RULE1> <RULE2> ...)    - like option, but doesn't consume the head keyword.
;;;                                       effectively makes the rule "virtual".
;;;
;;; A grammar is specified as a mapping from keywords to rules.
;;; A raw keyword on its own implicitly dispatches to it's associated rule.
;;; For example, a simple expression language might look like:
;;; '((:expr (option :add :mul :inc (literal)))
;;;   (:inc :expr)
;;;   (:add :expr :expr)
;;;   (:mul :expr :expr))

(defun match-once (ast rule rules)
  "Match the head of AST against RULE. Returns (values success matched remaining)."
  (cond
    ;; Bare keyword reference: consume one wrapped sub-node and recurse.
    ;; If the referenced rule is abstract (its body is a single (dispatch ...)
    ;; form), don't expect a wrapper — match the dispatch's inner rule
    ;; against the current AST position directly.
    ((keyword-ref-p rule)
     (unless (consp ast) (signal-match-error ast rule))
     (multiple-value-bind (sub-rule found) (get-rule rule rules)
       (unless found
         (signal-match-error ast rule))
       (let ((abstract-inner (abstract-rule-body sub-rule)))
         (cond
           (abstract-inner
            (match-once ast abstract-inner rules))
           ;; Concrete rule: descend into the wrapped sub-node.
           (t
            (let ((sub (car ast)))
              (unless (and (consp sub) (eq (car sub) rule))
                (signal-match-error ast rule))
              (match-node sub rule sub-rule rules)
              (values t sub (cdr ast))))))))

    ;; Combinator forms.
    ((combinator-p rule)
     (case (car rule)
       (keyword
        (if (and (consp ast) (eq (car ast) (cadr rule)))
            (values t (car ast) (cdr ast))
            (signal-match-error ast rule)))

       (identifier
        ;; A program identifier: any symbol that isn't a keyword and isn't nil.
        (if (and (consp ast)
                 (symbolp (car ast))
                 (not (keywordp (car ast)))
                 (not (null (car ast))))
            (values t (car ast) (cdr ast))
            (signal-match-error ast rule)))

       (symbol
        ;; Any symbol, including keywords and nil. Useful for meta-level
        ;; things like grammar rule names.
        (if (and (consp ast) (symbolp (car ast)))
            (values t (car ast) (cdr ast))
            (signal-match-error ast rule)))

       (literal
        (if (and (consp ast) (literal-p (car ast)))
            (values t (car ast) (cdr ast))
            (signal-match-error ast rule)))

       (maybe
        ;; Try the inner rule; on failure, succeed with no match consumed.
        (multiple-value-bind (success matched remaining)
            (try-match (match-once ast (cadr rule) rules))
          (if success
              (values t matched remaining)
              (values t nil ast))))

       (option
        ;; Try each alternative in order. First success wins.
        (loop for alt in (cdr rule) do
          (multiple-value-bind (success matched remaining)
              (try-match (match-once ast alt rules))
            (when success
              (return-from match-once (values t matched remaining))))
              finally (signal-match-error ast rule)))

       (repeat0
        ;; Greedily consume as many matches of the inner rule as possible.
        (let ((inner (cadr rule))
              (acc '())
              (cur ast))
          (loop
            (multiple-value-bind (success matched remaining)
                (try-match (match-once cur inner rules))
              (cond
                ((not success) (return))
                ;; Defensive: if the inner rule didn't actually consume input,
                ;; stop to avoid an infinite loop.
                ((eq remaining cur) (return))
                (t (push matched acc)
                   (setf cur remaining)))))
          (values t (nreverse acc) cur)))

       (dispatch
        ;; Marks an abstract rule. Match the inner form against AST directly
        ;; without expecting a wrapped sub-node. Used in rule bodies whose
        ;; purpose is to dispatch among concrete alternatives, like
        ;; :statement = (dispatch (option :declare :assign :expr ...)).
        (match-once ast (cadr rule) rules))))

    ;; Implicit sequence: any other list. Each element is a slot pattern
    ;; matched in order against consecutive AST elements at the current
    ;; level. This is the same semantics as a rule body.
    ((consp rule)
     (multiple-value-bind (success matched remaining)
         (match-sequence-prefix ast rule rules)
       (declare (ignore success))
       (values t matched remaining)))

    (t
     (signal-match-error ast rule))))

(defun match-sequence-prefix (ast body rules)
  "Match each form in BODY against AST in order. Does not error on trailing AST."
  (let ((cur ast)
        (results '()))
    (dolist (form body)
      (multiple-value-bind (success matched remaining)
          (match-once cur form rules)
        (declare (ignore success))   ; failure would have signaled
        (push matched results)
        (setf cur remaining)))
    (values t (nreverse results) cur)))

(defun match-sequence (ast body rules)
  "Match each form in BODY against AST in order. Errors on trailing AST."
  (multiple-value-bind (success matched remaining)
      (match-sequence-prefix ast body rules)
    (declare (ignore success))
    (unless (null remaining)
      (signal-match-error remaining body))
    (values t matched nil)))

(defun match-node (ast head body rules)
  "AST is expected to be (HEAD . tail). Match BODY against tail."
  (unless (and (consp ast) (eq (car ast) head))
    (signal-match-error ast (cons head body)))
  (match-sequence (cdr ast) body rules))

(defun match-grammar (ast top-level rules)
  "Match a top-level AST node against the rule named by TOP-LEVEL.
   Returns T on success; signals MATCH-ERROR on failure."
  (multiple-value-bind (body found) (get-rule top-level rules)
    (unless found
      (error "No rule for ~S in grammar." top-level))
    (let ((abstract-inner (abstract-rule-body body)))
      (cond
        ;; Abstract rule: match the dispatch's inner form against AST as if
        ;; AST were the next slot in a parent rule. We wrap AST in a list
        ;; so match-once sees it as one element to consume.
        (abstract-inner
         (multiple-value-bind (success matched remaining)
             (match-once (list ast) abstract-inner rules)
           (declare (ignore matched))
           (unless (and success (null remaining))
             (signal-match-error ast body))))
        ;; Concrete rule: AST must be (TOP-LEVEL . tail).
        (t
         (match-node ast top-level body rules))))
    t))

;;; ---------------------------------------------------------------------------
;;; The grammar
;;;
;;; Compound patterns that used to be inline anonymous sequences (like the
;;; binary operations in :expr) are now their own named rules. This keeps
;;; the matching semantics simple: every "thing" in the AST is a wrapped
;;; sub-node referenced by keyword.
;;; ---------------------------------------------------------------------------

(defparameter *blub-grammar*
  '((:module
     (repeat0 (option :function :global)))

    (:function
     :type (identifier) :args :block)

    (:args
     (repeat0 (:type (identifier))))

    (:block
     (repeat0 :statement))

    ;; Abstract: a statement is any of these concrete forms.
    (:statement
     (dispatch (option :declare
                       :assign
                       :expr
                       :if
                       :while
                       :return
                       :break
                       :continue)))

    (:declare
     :type (identifier) (maybe :expr))

    (:assign
     (identifier) :expr)

    (:global
     :type (identifier) (maybe :expr))

    ;; Control flow.
    (:if       :expr :block (maybe :block))   ; condition, then, optional else
    (:while    :expr :block)
    (:return   (maybe :expr))
    (:break)
    (:continue)

    (:type
     (option
      (keyword :void)
      (keyword :char)
      (keyword :int)
      (keyword :double)
      (keyword :boolean)
      :pointer))

    (:pointer :type)

    ;; Expressions. :expr dispatches to one concrete kind, with no wrapper.
    (:expr
     (dispatch
      (option
       (literal)
       :var
       (keyword :true)
       (keyword :false)
       ;; Unary
       :neg :not :deref :addr-of
       ;; Bitwise / arithmetic binary
       :add :sub :mul :div :and :or :xor
       ;; Comparison
       :eq :ne :lt :le :gt :ge
       ;; Logical
       :logand :logor
       ;; Function call
       :call)))

    (:var      (identifier))

    ;; Unary operators.
    (:neg      :expr)
    (:not      :expr)
    (:deref    :expr)
    (:addr-of  :expr)

    ;; Binary arithmetic / bitwise.
    (:add      :expr :expr)
    (:sub      :expr :expr)
    (:mul      :expr :expr)
    (:div      :expr :expr)
    (:and      :expr :expr)
    (:or       :expr :expr)
    (:xor      :expr :expr)

    ;; Comparisons.
    (:eq       :expr :expr)
    (:ne       :expr :expr)
    (:lt       :expr :expr)
    (:le       :expr :expr)
    (:gt       :expr :expr)
    (:ge       :expr :expr)

    ;; Logical (short-circuiting in C).
    (:logand   :expr :expr)
    (:logor    :expr :expr)

    ;; Function call: name followed by zero or more argument expressions.
    (:call     (identifier) (repeat0 :expr))))
