(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

;;; ===========================================================================
;;; Meta-grammar: a grammar that describes the grammar specification language.
;;;
;;; Our matcher requires every AST node to have a keyword as its head. The
;;; grammar specification language doesn't naturally fit this shape — it has
;;; combinator forms like (option ...) and (keyword :foo) headed by plain
;;; symbols, plus bare keyword rule references.
;;;
;;; To make the meta-grammar genuinely self-applicable, we translate any
;;; grammar into a keyword-headed AST first (see grammar->ast below), and
;;; then validate that AST against *grammar-grammar*.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Translator: surface grammar -> AST
;;; ---------------------------------------------------------------------------

(defun pattern->ast (pattern)
  "Translate one slot pattern from surface syntax to AST form."
  (cond
    ;; Rule reference: bare keyword.
    ((and (symbolp pattern) (keywordp pattern))
     (list :ref pattern))
    ;; Combinator forms.
    ((consp pattern)
     (case (car pattern)
       (option   (list :option (cons :patterns (mapcar #'pattern->ast (cdr pattern)))))
       (keyword  (list :kw (cadr pattern)))
       (identifier (list :identifier))
       (symbol     (list :symbol))
       (literal    (list :literal))
       (maybe    (list :maybe (pattern->ast (cadr pattern))))
       (repeat0  (list :repeat0 (pattern->ast (cadr pattern))))
       (dispatch (list :dispatch (pattern->ast (cadr pattern))))
       ;; Implicit sequence: not a recognized combinator head.
       (otherwise
        (cons :seq (mapcar #'pattern->ast pattern)))))
    (t (error "Unrecognized pattern: ~S" pattern))))

(defun rule->ast (rule)
  "Translate one rule definition (e.g. (:foo a b)) to AST form."
  (let ((name (car rule))
        (slots (cdr rule)))
    (list* :rule name (mapcar #'pattern->ast slots))))

(defun grammar->ast (grammar)
  "Translate a full grammar to the keyword-headed AST form expected by the
   matcher. The result has the shape (:grammar (:rule ...) (:rule ...) ...)."
  (cons :grammar (mapcar #'rule->ast grammar)))

;;; ---------------------------------------------------------------------------
;;; The meta-grammar itself
;;; ---------------------------------------------------------------------------

(defparameter *grammar-grammar*
  '((:grammar (repeat0 :rule))

    ;; A rule is a name (keyword) followed by zero or more slot patterns.
    (:rule (symbol) (repeat0 :pattern))

    ;; A pattern is one of the wrapped forms.
    (:pattern
     (dispatch (option :ref :option :kw :identifier :symbol
                       :literal :maybe :repeat0 :dispatch :seq)))

    (:ref        (symbol))                      ; (:ref :some-keyword)
    (:option     :patterns)                     ; (:option (:patterns p p p))
    (:patterns   (repeat0 :pattern))            ; (:patterns ...) holds the list
    (:kw         (symbol))                      ; (:kw :foo)
    (:identifier)                               ; (:identifier)
    (:symbol)                                   ; (:symbol)
    (:literal)                                  ; (:literal)
    (:maybe      :pattern)                      ; (:maybe x)
    (:repeat0    :pattern)                      ; (:repeat0 x)
    (:dispatch   :pattern)                      ; (:dispatch x)
    (:seq        (repeat0 :pattern))))          ; (:seq a b c)

;;; ---------------------------------------------------------------------------
;;; The self-applicable check
;;; ---------------------------------------------------------------------------

(defun validate-grammar (grammar)
  "Translate GRAMMAR to AST form and validate it against *grammar-grammar*.
   Returns T on success; signals MATCH-ERROR on failure."
  (match-grammar (grammar->ast grammar) :grammar *grammar-grammar*))
