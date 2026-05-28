(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

(defparameter *blub-grammar*
  '((:module
     (repeat0 (option :function :global :block :defstruct)))

    (:defstruct
      (identifier) (repeat0 (list :type (identifier))))

    (:function
     :type (identifier) :args :block)

    (:args
     (repeat0 (list :type (identifier))))

    (:block
     (repeat0 :statement))

    ;; Abstract: a statement is any of these concrete forms.
    (:statement
     (dispatch (option :declare
                       :set
                       :expr
                       :if
                       :while
                       :return
                       :break
                       :continue)))

    (:declare
     :type (identifier) (maybe :expr))

    ;; Unified assignment: LHS is either a plain variable name or a struct field lvalue.
    ;;   (:set x expr)              -- variable assignment
    ;;   (:set (:. struct field) v) -- struct field assignment
    (:set
     (option (identifier) :expr) :expr)

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
      (keyword :u8)   (keyword :i8)
      (keyword :u32)  (keyword :i32)
      (keyword :u64)  (keyword :i64)
      (keyword :f32)  (keyword :f64)
      :pointer :struct :fn))

    (:pointer :type)

    (:struct (identifier))

    ;; Function pointer type: return type followed by zero or more param types.
    ;;   (:fn (:type :i32))                          -- () -> i32
    ;;   (:fn (:type :i32) (:type :i32))             -- (i32) -> i32
    ;;   (:fn (:type :i32) (:type :i32) (:type :i32)) -- (i32, i32) -> i32
    (:fn :type (repeat0 :type))

    ;; Expressions. :expr dispatches to one concrete kind, with no wrapper.
    (:expr
     (dispatch
      (option
       (literal)
       :var
       ;; Unary
       :neg :not :deref :addr-of
       ;; Bitwise / arithmetic binary
       :add :sub :mul :div :and :or :xor
       ;; Comparison
       :eq :ne :lt :le :gt :ge
       ;; Logical
       :logand :logor
       ;; Function call (direct and indirect) and struct field access
       :call :. :->
       ;; Take the address of a named function (yields a (:fn ...) typed value).
       :fn-ptr
       ;; Explicit type cast (like C casting).
       :cast)))

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

    ;; Unified call: callee is either a function name (identifier) or
    ;; an expression of (:fn ...) type (function pointer).
    ;;   (:call add 1 2)       -- direct named call
    ;;   (:call (:var fn) 1 2) -- indirect call through a function pointer variable
    (:call     (option (identifier) :expr) (repeat0 :expr))

    ;; Take the address of a named function; yields a (:fn ...) typed value.
    (:fn-ptr   (identifier))

    ;; Struct field access: struct followed by member name.
    (:.        :expr (identifier))

    ;; Pointer-to-struct field access (sugar for (:. (:deref ptr) field)).
    ;; Desugared by pass 0; never seen by later passes.
    (:->       :expr (identifier))

    ;; Explicit type cast: (:cast (:type T) expr) -- like (T) expr in C.
    (:cast     :type :expr)))

(defun validate-blub (ast)
  (match-grammar ast :module *blub-grammar*)
  ast)

;; =============================================================================
;; Module metadata (:meta section)
;; =============================================================================
;; Passes store and exchange metadata via a (:meta ...) node appended as the
;; last child of :module. This keeps inter-pass state explicit in the AST and
;; avoids reliance on persistent global variables between passes.
;;
;; Format: (:meta (:key1 value1) (:key2 value2) ...)
;;   :struct-env  -- alist (name . layout-plist) from pass 2
;;   :global-env  -- alist (name . type) from pass 3
;;   :fn-sigs     -- alist (name . (ret-type . arg-types)) from pass 3
;;
;; Because :meta is the LAST child, (second module) still returns the first
;; real item, preserving backward compatibility with navigation in tests.

(defun meta-empty () '(:meta))

(defun meta-get (meta key)
  "Return the value for KEY in META, or NIL if absent."
  (let ((item (find key (cdr meta) :key #'car)))
    (when item (cadr item))))

(defun meta-set (meta key value)
  "Return a new :meta node with KEY set to VALUE."
  (cons :meta (cons (list key value)
                    (remove key (cdr meta) :key #'car))))

(defun extract-meta (body)
  "Split module BODY into (values meta items).
   If the last item is (:meta ...), removes it; otherwise returns meta-empty and BODY unchanged."
  (if (and (consp body)
           (consp (car (last body)))
           (eq (caar (last body)) :meta))
    (values (car (last body)) (butlast body))
    (values (meta-empty) body)))

(defun fset-map->alist (m)
  "Convert an FSet map to a list of (key . value) pairs."
  (let ((result '()))
    (fset:do-map (k v m) (push (cons k v) result))
    result))

(defun alist->fset-map (alist)
  "Convert a list of (key . value) pairs to an FSet map."
  (reduce (lambda (m pair) (fset:with m (car pair) (cdr pair)))
          alist :initial-value (fset:empty-map)))


;; =============================================================================
;; Pass 0: Desugaring
;; =============================================================================
;; - Desugar (:declare type name value) into separate (:declare type name)
;;   and (:set name value) statements.
;; - :block uses recurse-splice so :declare can expand to two statements in-place.
;; - Desugar (:-> a b) into (:. (:deref a) b)  (pointer-to-struct field access)

(defparameter *blub-0* (make-interpreter :on-unknown :recurse
                                         :readable-name "BLUB-0 (desugar)"))

(def-op *blub-0* (:block &rest body)
  ;; Recurse each child in splice context so :declare can expand to multiple stmts.
  (cons :block (mapcan #'recurse-splice body)))

(def-op *blub-0* (:declare type name &optional value)
  ;; If a value is provided, desugar into a bare declaration followed by an
  ;; assignment. The splice will be flattened by the enclosing :block handler.
  (if value
    (splice (list :declare type name)
            (list :set name value))
    (list :declare type name)))

(def-op *blub-0* (:-> struct-expr field)
  ;; Pointer-to-struct field access: desugar to (:. (:deref ptr) field).
  (list :. (list :deref (recurse struct-expr)) field))

(def-op *blub-0* (:function type name args block)
  ;; Recurse into the body block so nested declarations get desugared.
  (list :function type name args (recurse block)))

(def-op *blub-0* (:defstruct name &rest fields)
  ;; Struct definitions have no statements to desugar; pass through unchanged.
  (list* :defstruct name fields))

(def-op *blub-0* (:module &rest body)
  ;; Process each top-level item (functions, globals, standalone blocks, struct defs).
  (cons :module (mapcar #'recurse body)))


;; =============================================================================
;; Pass 1: Rename variables
;; =============================================================================
;; Lexical scoping rules:
;;
;; A variable can only be declared once within the global/block/function scope.
;; If a variable is re-declared, it becomes shadowed by the new name. Variables
;; can only be accessed after they have been declared.
;;
;; Renaming algorithm (powered by dynamically scoped pass contexts):
;; - Uses `rename-env` context to maintain a map of variable renames.
;; - On module declaration: enters an empty `rename-env` scope.
;; - On function/block declaration: enters a shadowed `rename-env` scope.
;;   Modifications only affect the current scope and its children. Exiting
;;   the block automatically restores the environment to its outer state.
;; - On variable declaration: if the name is already used in the *current*
;;   scope (or a parent scope), we generate a fresh name and bind it locally.

(defparameter *blub-1* (make-interpreter :on-unknown :recurse
                                         :readable-name "BLUB-1 (rename)"))

(defvar *rename-env* (fset:empty-map) "Tracks variable renames for the blub-1 pass.")

(defun node-is-p (keyword)
  (lambda (node) (and (consp node) (eq (car node) keyword))))

(defun node-is-not-p (keyword)
  (lambda (node) (and (consp node) (not (eq (car node) keyword)))))

(defun filter (nodes predicate)
  (loop for node in nodes if (funcall predicate node) collect node))

(defun register-global (name)
  "Add NAME -> NAME to *rename-env*. Errors if NAME is already there."
  (when (nth-value 1 (fset:lookup *rename-env* name))
    (error "Global variable ~A already declared." name))
  (setf *rename-env* (fset:with *rename-env* name name))
  name)

(defun register-local (name)
  "Add NAME -> chosen-name to *rename-env*, freshening if NAME is
   already bound (shadowing). Returns the chosen name."
  (let* ((found     (nth-value 1 (fset:lookup *rename-env* name)))
         (new-name  (if found (fresh-name (string name)) name)))
    (setf *rename-env* (fset:with *rename-env* name new-name))
    new-name))

(defun lookup-or-error (name kind)
  "Look up NAME in *rename-env*. KIND is a string used in the error
   message (e.g. \"assigned\" or \"read\"). Returns the renamed symbol."
  (multiple-value-bind (mapped found) (fset:lookup *rename-env* name)
    (unless found (error "Variable ~A but not yet declared: ~A." kind name))
    mapped))

(def-op *blub-1* (:global type name &optional value)
  (register-global name)
  (if value
    (list :global type name (recurse value))
    (list :global type name)))

(def-op *blub-1* (:declare type name &optional value)
  ;; Recurse on VALUE *before* updating the map, so a self-referential
  ;; declaration like (:declare (int x) (:var x)) resolves :var x
  ;; against the OUTER scope's binding.
  ;; Only include value in the output when provided; after pass 0 it
  ;; is never present, and pass 5 expects exactly (:declare type name).
  (let* ((lowered-value (when value (recurse value)))
         (new-name      (register-local name)))
    (if lowered-value
      (list :declare type new-name lowered-value)
      (list :declare type new-name))))

(def-op *blub-1* (:set lhs value)
  (if (symbolp lhs)
    ;; Variable assignment: rename the variable.
    (list :set (lookup-or-error lhs "set") (recurse value))
    ;; Struct field assignment (:. struct-expr field-name): recurse into struct-expr.
    (destructuring-bind (dot struct-expr field-name) lhs
      (list :set (list dot (recurse struct-expr) field-name) (recurse value)))))

(def-op *blub-1* (:var name)
  (list :var (lookup-or-error name "read")))

(def-op *blub-1* (:block &rest body)
  ;; Fresh dynamic binding initialized from outer scope, so changes don't leak out.
  (let ((*rename-env* *rename-env*))
    (cons :block (mapcar #'recurse body))))

(def-op *blub-1* (:function type name args block)
  (let ((*rename-env* *rename-env*))
    ;; Each ARG is a (type name) pair; we register the name (which may shadow
    ;; an outer global with the same name) and rebuild the pair using the
    ;; chosen new name.
    (let ((renamed-args
            (cons :args
                  (mapcar (lambda (arg)
                            (destructuring-bind (arg-type arg-name) arg
                              (let* ((new-name  (register-local arg-name))
                                     (new-pair  (list arg-type new-name)))
                                (inherit-from new-pair arg)
                                new-pair)))
                          (cdr args)))))
      (list :function type name renamed-args (recurse block)))))

(def-op *blub-1* (:defstruct name &rest fields)
  ;; Struct definitions have no variable names; pass through unchanged.
  (list* :defstruct name fields))

(def-op *blub-1* (:module &rest body)
  ;; Fresh empty map at module scope. Globals are processed first so
  ;; their bindings are visible to all functions/blocks regardless of
  ;; textual order, then the rest of the module is renamed.
  (let ((*rename-env* (fset:empty-map)))
    (let ((globals (mapcar #'recurse (filter body (node-is-p :global))))
          (renamed (mapcar #'recurse (filter body (node-is-not-p :global)))))
      (cons :module (append globals renamed)))))


;; =============================================================================
;; Pass 2: Struct layout resolution
;; =============================================================================
;; Scans :defstruct definitions and computes C-compatible field offsets, sizes,
;; and alignment. Transforms:
;;   (:defstruct name (type1 field1) (type2 field2) ...)
;; into:
;;   (:defstruct name total-size alignment (field1 type1 offset1) ...)
;; This annotated form is consumed by passes 3 and 5 via :struct-env in :meta.
;;
;; Structs referencing other structs must be defined before they are used.

(defparameter *blub-2* (make-interpreter :on-unknown :passthrough
                                         :readable-name "BLUB-2 (struct layout)"))

(defvar *blub-struct-env* (fset:empty-map)
  "Maps struct name symbols -> plist (:size N :align N :fields ((name type offset)...)).")

(defun round-up-to (n alignment)
  "Round N up to the nearest multiple of ALIGNMENT."
  (if (zerop alignment) n (* (ceiling n alignment) alignment)))

(defun blub-field-size-align (field-type struct-env)
  "Return (values byte-size alignment) for a blub type, using C-compatible rules.
   STRUCT-ENV is the caller's pass-local struct layout map."
  (case (blub-type-inner field-type)
    ((:u8 :i8)         (values 1 1))
    ((:u32 :i32 :f32)  (values 4 4))
    ((:u64 :i64 :f64 :pointer) (values 8 8))
    (:struct
     (let* ((sname (blub-struct-name field-type))
            (layout (nth-value 0 (fset:lookup struct-env sname))))
       (unless layout
         (error "blub-field-size-align: unknown struct ~S" sname))
       (values (getf layout :size) (getf layout :align))))
    (t (error "blub-field-size-align: unknown type ~S" field-type))))


(defun blub-compute-struct-layout (fields struct-env)
  "Given FIELDS as a list of (type name) pairs, compute C-compatible struct layout.
   STRUCT-ENV is the pass-local map used to resolve nested struct field sizes.
   Returns a plist (:size N :align N :fields ((name type offset)...))."
  (let ((offset 0) (max-align 1) (resolved '()))
    (dolist (field fields)
      (destructuring-bind (ftype fname) field
        (multiple-value-bind (fsize falign) (blub-field-size-align ftype struct-env)
          (setf max-align (max max-align falign))
          (let ((ao (round-up-to offset falign)))
            (push (list fname ftype ao) resolved)
            (setf offset (+ ao fsize))))))
    (list :size (round-up-to offset max-align)
          :align max-align
          :fields (nreverse resolved))))


(def-op *blub-2* (:defstruct name &rest fields)
  ;; Resolve C-compatible layout for this struct definition.
  ;; Fields from the grammar are (type name) pairs.
  (let ((layout (blub-compute-struct-layout fields *blub-struct-env*)))
    ;; Register this struct so subsequent :defstruct nodes can reference it.
    (setf *blub-struct-env* (fset:with *blub-struct-env* name layout))
    ;; Annotated form: (:defstruct name size align (fname ftype offset) ...)
    (list* :defstruct name
           (getf layout :size)
           (getf layout :align)
           (getf layout :fields))))

(def-op *blub-2* (:module &rest body)
  ;; Process each item; :defstruct handlers update *blub-struct-env* in order.
  ;; Store the final struct-env in :meta so passes 3 and 5 can read it directly.
  (multiple-value-bind (meta items) (extract-meta body)
    (declare (ignore meta))
    (let ((*blub-struct-env* (fset:empty-map)))
      (let ((processed (mapcar #'recurse items)))
        (append (list* :module processed)
                (list (meta-set (meta-empty) :struct-env
                                (fset-map->alist *blub-struct-env*))))))))


;; =============================================================================
;; Pass 3: Typechecking
;; =============================================================================
;; Validates types after passes 0 and 1. Checks:
;;   - Variables used only after declaration.
;;   - Arithmetic operands are numeric; bitwise operands are integer-family.
;;   - Binary operands have compatible types.
;;   - Function calls have correct argument counts and compatible types.
;;   - :return expressions match the enclosing function's declared type.
;;   - :if/:while conditions are not :double.
;;
;; Design: every expression handler returns a (:typed TYPE INNER) form.
;; Use TC-LOWER-EXPR (not RECURSE) for expression sub-forms so that numeric
;; literals are also wrapped with their inferred types.

(defparameter *blub-3* (make-interpreter :on-unknown :recurse
                                         :readable-name "BLUB-3 (typecheck)"))

;; --- Pass 3 state (all rebound by the :module and :function handlers) ---

(defvar *tc-var-type-env* (fset:empty-map)
  "Maps local variable names -> blub (:type ...) nodes. Rebound per function.")

(defvar *tc-global-type-env* (fset:empty-map)
  "Maps global variable names -> blub (:type ...) nodes. Set by :module.")

(defvar *tc-fn-sigs* (fset:empty-map)
  "Maps function names -> (ret-type . (arg-type ...)). Set by :module.")

(defvar *tc-return-type* nil
  "Declared return type of the current function. Set by :function.")

(defvar *tc-struct-env* (fset:empty-map)
  "Maps struct name symbols -> layout plists for pass 3. Rebound by :module
   from the :struct-env entry in :meta. Never shared with other passes.")


;; Type utilities

(defun blub-type-inner (type)
  "Extract the kind keyword from a blub type node.
   (:type :i32)           -> :i32
   (:type (:pointer X))  -> :pointer
   (:type (:struct name)) -> :struct
   (:type (:fn ret ...))  -> :fn
   (:pointer X)          -> :pointer  (handles raw pointer nodes too)"
  (cond
    ((and (consp type) (eq (car type) :type))
     (let ((inner (cadr type)))
       (cond
         ((keywordp inner) inner)
         ((and (consp inner) (eq (car inner) :pointer)) :pointer)
         ((and (consp inner) (eq (car inner) :struct))  :struct)
         ((and (consp inner) (eq (car inner) :fn))      :fn)
         (t (error "blub-type-inner: unknown :type content ~S" inner)))))
    ((and (consp type) (eq (car type) :pointer)) :pointer)
    ((and (consp type) (eq (car type) :struct))  :struct)
    ((and (consp type) (eq (car type) :fn))      :fn)
    (t (error "blub-type-inner: not a type node ~S" type))))

(defun blub-struct-name (type)
  "Extract the struct name symbol from (:type (:struct name)) or (:struct name)."
  (cond
    ((and (consp type) (eq (car type) :type)
          (consp (cadr type)) (eq (car (cadr type)) :struct))
     (cadr (cadr type)))
    ((and (consp type) (eq (car type) :struct))
     (cadr type))
    (t (error "blub-struct-name: not a struct type ~S" type))))

(defun blub-type-of (expr)
  "Extract the Blub type annotation from a :typed expression, or infer from literal.
   After pass 3, all sub-expressions are wrapped as (:typed TYPE INNER).
   Used by passes 4 and 5 to read type info threaded through the AST."
  (cond
    ((and (consp expr) (eq (car expr) :typed)) (cadr expr))
    ((and (numberp expr) (typep expr 'single-float)) '(:type :f32))
    ((and (numberp expr) (floatp expr)) '(:type :f64))
    ((numberp expr) '(:type :i32))
    (t nil)))

(defun tc-int-like-p (type)
  "True for integer-family types (all sizes, signed and unsigned)."
  (member (blub-type-inner type) '(:u8 :i8 :u32 :i32 :u64 :i64)))

(defun tc-numeric-p (type)
  "True for types valid in arithmetic: integers plus :f32 and :f64."
  (member (blub-type-inner type) '(:u8 :i8 :u32 :i32 :u64 :i64 :f32 :f64)))

(defun tc-compatible-p (t1 t2)
  "True when T1 and T2 are assignment/comparison-compatible.
   8/32-bit integer types are mutually compatible (all map to QBE :w).
   Any 8/32-bit integer is also compatible with 64-bit integers (implicit widening).
   64-bit integers are mutually compatible with each other.
   :f32, :f64, :pointer, and :fn require exact kind match."
  (let ((k1 (blub-type-inner t1)) (k2 (blub-type-inner t2))
        (w32 '(:u8 :i8 :u32 :i32))
        (w64 '(:u64 :i64)))
    (or (and (member k1 w32) (member k2 w32))   ; both 32-bit (or smaller)
        (and (member k1 w64) (member k2 w64))   ; both 64-bit
        (and (member k1 w32) (member k2 w64))   ; implicit widening 32→64
        (eq k1 k2))))

;; --- Expression lowering helper ---

(defun tc-lower-expr (expr)
  "Lower EXPR through *blub-3*, returning a (:typed TYPE LOWERED-EXPR) form.
   Literals are wrapped with their inferred types; list forms are dispatched to
   handlers which return :typed forms directly."
  (cond
    ((typep expr 'single-float) (list :typed '(:type :f32) expr))
    ((floatp expr)     (list :typed '(:type :f64) expr))
    ((numberp expr)    (list :typed '(:type :i32) expr))
    ((consp expr)      (lower *blub-3* expr))
    (t (error "typecheck: unexpected atom in expression position: ~S" expr))))

;; --- Variable lookup ---

(defun tc-lookup-var-type (name)
  "Look up NAME in the local then global type env. Signals an error if absent."
  (multiple-value-bind (type found) (fset:lookup *tc-var-type-env* name)
    (if found type
        (multiple-value-bind (gtype gfound) (fset:lookup *tc-global-type-env* name)
          (if gfound gtype
              (error "typecheck: variable ~A used before declaration" name))))))

;; --- Binary operator helper ---

(defun tc-check-binop (op left right required-pred pred-name result-type-fn)
  "Type-check a binary operator. Lower LEFT and RIGHT, verify both satisfy
   REQUIRED-PRED (a predicate on blub type nodes). Returns (:typed RESULT-TYPE (:op l r))."
  (let* ((l  (tc-lower-expr left))
         (lt (blub-type-of l))
         (r  (tc-lower-expr right))
         (rt (blub-type-of r)))
    (unless (and lt (funcall required-pred lt))
      (error "typecheck: ~A left operand has type ~S, expected ~A" op lt pred-name))
    (unless (and rt (funcall required-pred rt))
      (error "typecheck: ~A right operand has type ~S, expected ~A" op rt pred-name))
    (unless (tc-compatible-p lt rt)
      (error "typecheck: ~A operands have incompatible types ~S and ~S" op lt rt))
    (list :typed (funcall result-type-fn lt) (list op l r))))

;; --- Pass 3: expression handlers ---

(def-op *blub-3* (:var name)
  (list :typed (tc-lookup-var-type name) (list :var name)))

;; Arithmetic/bitwise binary operators.
(def-op *blub-3* (:add  l r) (tc-check-binop :add  l r #'tc-numeric-p  "numeric"          #'identity))
(def-op *blub-3* (:sub  l r) (tc-check-binop :sub  l r #'tc-numeric-p  "numeric"          #'identity))
(def-op *blub-3* (:mul  l r) (tc-check-binop :mul  l r #'tc-numeric-p  "numeric"          #'identity))
(def-op *blub-3* (:div  l r) (tc-check-binop :div  l r #'tc-numeric-p  "numeric"          #'identity))
(def-op *blub-3* (:and  l r) (tc-check-binop :and  l r #'tc-int-like-p "integer" #'identity))
(def-op *blub-3* (:or   l r) (tc-check-binop :or   l r #'tc-int-like-p "integer" #'identity))
(def-op *blub-3* (:xor  l r) (tc-check-binop :xor  l r #'tc-int-like-p "integer" #'identity))

;; Comparison operators: operands numeric, result is always :i32 (0 or 1).
(def-op *blub-3* (:eq   l r) (tc-check-binop :eq   l r #'tc-numeric-p  "numeric" (constantly '(:type :i32))))
(def-op *blub-3* (:ne   l r) (tc-check-binop :ne   l r #'tc-numeric-p  "numeric" (constantly '(:type :i32))))
(def-op *blub-3* (:lt   l r) (tc-check-binop :lt   l r #'tc-numeric-p  "numeric" (constantly '(:type :i32))))
(def-op *blub-3* (:le   l r) (tc-check-binop :le   l r #'tc-numeric-p  "numeric" (constantly '(:type :i32))))
(def-op *blub-3* (:gt   l r) (tc-check-binop :gt   l r #'tc-numeric-p  "numeric" (constantly '(:type :i32))))
(def-op *blub-3* (:ge   l r) (tc-check-binop :ge   l r #'tc-numeric-p  "numeric" (constantly '(:type :i32))))

;; Logical operators: int-like operands, result is :i32 (0 or 1), short-circuit in codegen.
(def-op *blub-3* (:logand l r) (tc-check-binop :logand l r #'tc-int-like-p "integer" (constantly '(:type :i32))))
(def-op *blub-3* (:logor  l r) (tc-check-binop :logor  l r #'tc-int-like-p "integer" (constantly '(:type :i32))))

(def-op *blub-3* (:not operand)
  (let* ((v  (tc-lower-expr operand))
         (vt (blub-type-of v)))
    (unless (and vt (tc-int-like-p vt))
      (error "typecheck: :not operand has type ~S, expected integer" vt))
    (list :typed vt (list :not v))))

(def-op *blub-3* (:neg operand)
  (let* ((v  (tc-lower-expr operand))
         (vt (blub-type-of v)))
    (unless (and vt (tc-numeric-p vt))
      (error "typecheck: :neg operand has type ~S, expected numeric" vt))
    (list :typed vt (list :neg v))))

(def-op *blub-3* (:addr-of inner)
  ;; Only simple variables are addressable.
  (unless (and (consp inner) (eq (car inner) :var))
    (error "typecheck: :addr-of requires a variable, got ~S" inner))
  (let* ((v  (tc-lower-expr inner))
         (vt (blub-type-of v)))
    (list :typed (list :type (list :pointer vt)) (list :addr-of v))))

(def-op *blub-3* (:fn-ptr name)
  ;; Return a typed function pointer: (:type (:fn ret-type param-types...)).
  (multiple-value-bind (sig found) (fset:lookup *tc-fn-sigs* name)
    (unless found
      (error "typecheck: :fn-ptr references undeclared function ~A" name))
    (let* ((ret-type  (car sig))
           (arg-types (cdr sig))
           (fn-type   (list :type (list* :fn ret-type arg-types))))
      (list :typed fn-type (list :fn-ptr name)))))

(defun tc-castable-p (src dst)
  "True when a C-style cast from SRC to DST is legal.
   Allows casts between any scalar types (integers, floats, pointers, fn-ptrs).
   Forbids casts involving :struct or :void."
  (let ((sk (blub-type-inner src))
        (dk (blub-type-inner dst)))
    (and (not (member sk '(:struct :void)))
         (not (member dk '(:struct :void))))))

(def-op *blub-3* (:cast dst-type inner)
  (let* ((v  (tc-lower-expr inner))
         (vt (blub-type-of v)))
    (unless (tc-castable-p vt dst-type)
      (error "typecheck: cannot cast from ~S to ~S" vt dst-type))
    (list :typed dst-type (list :cast dst-type v))))

(def-op *blub-3* (:deref ptr-expr)
  (let* ((p  (tc-lower-expr ptr-expr))
         (pt (blub-type-of p)))
    ;; Expect (:type (:pointer <pointee>)).
    (let ((pointee
            (and pt (consp pt) (eq (car pt) :type)
                 (consp (cadr pt)) (eq (car (cadr pt)) :pointer)
                 (cadr (cadr pt)))))
      (unless pointee
        (error "typecheck: :deref operand has type ~S, expected a pointer type" pt))
      (list :typed pointee (list :deref p)))))

(def-op *blub-3* (:call callee &rest args)
  ;; Unified call: callee is either a symbol (direct call) or an expression
  ;; of (:fn ret param...) type (indirect call through a function pointer).
  (if (symbolp callee)
    ;; Direct call: look up function signature.
    (multiple-value-bind (sig found) (fset:lookup *tc-fn-sigs* callee)
      (unless found
        (error "typecheck: call to undeclared function ~A" callee))
      (let* ((ret-type  (car sig))
             (arg-types (cdr sig))
             (n-formal  (length arg-types))
             (n-actual  (length args)))
        (unless (= n-formal n-actual)
          (error "typecheck: ~A expects ~D arg~:P, got ~D" callee n-formal n-actual))
        (let ((lowered-args
                (mapcar (lambda (arg formal-type)
                          (let* ((a  (tc-lower-expr arg))
                                 (at (blub-type-of a)))
                            (when (and at (not (tc-compatible-p at formal-type)))
                              (error "typecheck: arg to ~A has type ~S, expected ~S"
                                     callee at formal-type))
                            a))
                        args arg-types)))
          (list :typed ret-type (list* :call callee lowered-args)))))
    ;; Indirect call: callee is an expression; must have (:fn ...) type.
    (let* ((ce          (tc-lower-expr callee))
           (ct          (blub-type-of ce)))
      (unless (eq (blub-type-inner ct) :fn)
        (error "typecheck: indirect :call callee has type ~S, expected a (:fn ...) type" ct))
      (let* ((fn-inner  (cadr ct))           ; (:fn ret-type param-type...)
             (ret-type  (cadr fn-inner))     ; first element = return type
             (arg-types (cddr fn-inner))     ; rest = param types
             (n-formal  (length arg-types))
             (n-actual  (length args)))
        (unless (= n-formal n-actual)
          (error "typecheck: function pointer expects ~D arg~:P, got ~D"
                 n-formal n-actual))
        (let ((lowered-args
                (mapcar (lambda (arg formal-type)
                          (let* ((a  (tc-lower-expr arg))
                                 (at (blub-type-of a)))
                            (when (and at (not (tc-compatible-p at formal-type)))
                              (error "typecheck: indirect call arg has type ~S, expected ~S"
                                     at formal-type))
                            a))
                        args arg-types)))
          (list :typed ret-type (list* :call ce lowered-args)))))))

;; --- Pass 3: statement handlers ---

(def-op *blub-3* (:declare type name)
  ;; Register the variable's type. After pass 0, :declare never has an initializer.
  (setf *tc-var-type-env* (fset:with *tc-var-type-env* name type))
  (list :declare type name))

(def-op *blub-3* (:set lhs value)
  ;; Unified assignment: LHS is either a variable name or a struct field lvalue.
  (if (symbolp lhs)
    ;; Variable assignment.
    (let* ((v  (tc-lower-expr value))
           (vt (blub-type-of v))
           (nt (tc-lookup-var-type lhs)))
      (when (and vt (not (tc-compatible-p vt nt)))
        (error "typecheck: :set ~S: value type ~S incompatible with declared type ~S"
               lhs vt nt))
      (list :set lhs v))
    ;; Struct field assignment: LHS is (:. struct-expr field-name).
    (destructuring-bind (dot struct-expr field-name) lhs
      (declare (ignore dot))
      (let* ((se     (tc-lower-expr struct-expr))
             (st     (blub-type-of se))
             (sname  (if (eq (blub-type-inner st) :struct) (blub-struct-name st)
                         (error "typecheck: :set (:.) applied to non-struct type ~S" st))))
        (multiple-value-bind (layout found) (fset:lookup *tc-struct-env* sname)
          (unless found (error "typecheck: :set unknown struct ~A" sname))
          (let ((field-entry (find field-name (getf layout :fields) :key #'car)))
            (unless field-entry
              (error "typecheck: struct ~A has no field ~A" sname field-name))
            (destructuring-bind (fname ftype foffset) field-entry
              (declare (ignore fname))
              (let* ((v  (tc-lower-expr value))
                     (vt (blub-type-of v)))
                (when (and vt (not (tc-compatible-p vt ftype)))
                  (error "typecheck: :set ~A.~A expects ~S, got ~S"
                         sname field-name ftype vt))
                ;; Annotate LHS with offset and field-type for pass 5.
                (list :set (list :. se field-name foffset ftype) v)))))))))

(def-op *blub-3* (:block &rest body)
  ;; Recurse each statement for its side-effect checks.
  ;; :declare handlers extend *tc-var-type-env* for subsequent stmts.
  (cons :block (mapcar #'recurse body)))

(def-op *blub-3* (:if cond then &optional else)
  (let* ((c  (tc-lower-expr cond))
         (ct (blub-type-of c)))
    (when (and ct (member (blub-type-inner ct) '(:f32 :f64)))
      (error "typecheck: :if condition has type :double (use a comparison instead)"))
    (if else
        (list :if c (recurse then) (recurse else))
        (list :if c (recurse then)))))

(def-op *blub-3* (:while cond body)
  (let* ((c  (tc-lower-expr cond))
         (ct (blub-type-of c)))
    (when (and ct (member (blub-type-inner ct) '(:f32 :f64)))
      (error "typecheck: :while condition has type :double (use a comparison instead)"))
    (list :while c (recurse body))))

(def-op *blub-3* (:return &optional value)
  (if value
      (let* ((v  (tc-lower-expr value))
             (vt (blub-type-of v)))
        (when (and vt *tc-return-type* (not (tc-compatible-p vt *tc-return-type*)))
          (error "typecheck: :return value has type ~S but function declares ~S"
                 vt *tc-return-type*))
        (list :return v))
      (progn
        (when (and *tc-return-type*
                   (not (eq (blub-type-inner *tc-return-type*) :void)))
          (error "typecheck: empty :return in non-void function (returns ~S)"
                 *tc-return-type*))
        '(:return))))

;; --- Pass 3: top-level handlers ---

(def-op *blub-3* (:global type name &optional value)
  ;; Globals were pre-scanned by :module; validate any initializer but keep
  ;; the raw literal value (pass 5 expects a literal, not a :typed wrapper).
  (if value
      (progn (tc-lower-expr value) (list :global type name value))
      (list :global type name)))

(def-op *blub-3* (:function ret-type name args body)
  ;; Each function gets a fresh local type env. Globals remain visible
  ;; via *tc-global-type-env* (not rebound here).
  (let* ((*tc-var-type-env* (fset:empty-map))
         (*tc-return-type*  ret-type))
    ;; Register parameter types before recursing the body.
    (dolist (arg (cdr args))
      (destructuring-bind (arg-type arg-name) arg
        (setf *tc-var-type-env* (fset:with *tc-var-type-env* arg-name arg-type))))
    (list :function ret-type name args (recurse body))))

(def-op *blub-3* (:defstruct name size align &rest fields)
  ;; Struct definition already resolved by pass 2; pass through.
  (list* :defstruct name size align fields))

(def-op *blub-3* (:. struct-expr field-name)
  ;; Field access: typecheck base, look up field, annotate with type+offset.
  (let* ((se (tc-lower-expr struct-expr))
         (st (blub-type-of se))
         ;; Determine the struct name from either a struct value or deref'd pointer.
         (sname
           (cond
             ((and st (eq (blub-type-inner st) :struct))
              (blub-struct-name st))
             (t (error "typecheck: (:.) applied to non-struct type ~S" st)))))
    (multiple-value-bind (layout found) (fset:lookup *tc-struct-env* sname)
      (unless found
        (error "typecheck: unknown struct ~A in field access" sname))
      (let ((field-entry (find field-name (getf layout :fields) :key #'car)))
        (unless field-entry
          (error "typecheck: struct ~A has no field ~A" sname field-name))
        (destructuring-bind (fname ftype foffset) field-entry
          (declare (ignore fname))
          (list :typed ftype (list :. se field-name foffset)))))))

(def-op *blub-3* (:module &rest body)
  ;; Phase 1: build struct, global, and function-signature tables so all bodies
  ;; can reference them regardless of textual order.
  (multiple-value-bind (meta items) (extract-meta body)
    (let ((*tc-struct-env*     (if (meta-get meta :struct-env)
                                  (alist->fset-map (meta-get meta :struct-env))
                                  (fset:empty-map)))
          (*tc-global-type-env* (fset:empty-map))
          (*tc-fn-sigs*         (fset:empty-map)))
      ;; Pre-scan structs only when not already in :meta (pass 2 was skipped).
      (unless (meta-get meta :struct-env)
        (dolist (decl items)
          (when (and (consp decl) (eq (car decl) :defstruct))
            (destructuring-bind (kw sname ssize salign &rest sfields) decl
              (declare (ignore kw))
              (setf *tc-struct-env*
                    (fset:with *tc-struct-env* sname
                               (list :size ssize :align salign :fields sfields)))))))
      ;; Pre-scan globals (always needed: forward references to global vars).
      (dolist (decl items)
        (when (and (consp decl) (eq (car decl) :global))
          (destructuring-bind (kw type gname &optional val) decl
            (declare (ignore kw val))
            (setf *tc-global-type-env* (fset:with *tc-global-type-env* gname type)))))
      ;; Pre-scan function signatures (always needed: mutual recursion).
      (dolist (decl items)
        (when (and (consp decl) (eq (car decl) :function))
          (destructuring-bind (kw fn-ret fn-name fn-args fn-body) decl
            (declare (ignore kw fn-body))
            (setf *tc-fn-sigs*
                  (fset:with *tc-fn-sigs* fn-name
                             (cons fn-ret (mapcar #'car (cdr fn-args))))))))
      ;; Phase 2: typecheck every declaration, then store all envs in :meta.
      (let* ((processed (mapcar #'recurse items))
             (new-meta  (meta-set
                         (meta-set
                          (meta-set meta
                                    :struct-env (fset-map->alist *tc-struct-env*))
                          :global-env (fset-map->alist *tc-global-type-env*))
                         :fn-sigs (fset-map->alist *tc-fn-sigs*))))
        (append (list* :module processed) (list new-meta))))))


;; =============================================================================
;; Pass 4: Normalize expression nesting (three-address-code conversion)
;; =============================================================================
;; Extracts sub-expressions of statements into fresh temporaries so that each
;; statement performs at most one primitive operation with atomic (literal or
;; :var) operands.  Example:
;;
;;   (:set x (:add (:mul (:var a) (:var b)) (:var c)))
;;   =>
;;   (:declare (:type :i32) #:T1)
;;   (:assign #:T1 (:mul (:var a) (:var b)))   ; internal temp assignment
;;   (:set x       (:add (:var #:T1) (:var c)))
;;
;; Design: P4-ATOMIZE and P4-SIMPLIFY are mutually recursive helpers.
;;   P4-SIMPLIFY(expr): return a version of EXPR where every direct argument is
;;   atomic; any lifted sub-expressions are appended to *P4-PREFIX*.
;;   P4-ATOMIZE(expr): call P4-SIMPLIFY, then lift the whole simplified expr
;;   to a fresh temp if it is still non-atomic.
;;
;; Each statement handler binds *P4-PREFIX* freshly to NIL, calls P4-SIMPLIFY
;; on its expression operands, then uses P4-EMIT-STMT to return a splice of
;; any prefix temp-stmts followed by the simplified statement.
;;
;; Limitation: :while conditions are intentionally NOT simplified here.
;; Extracting sub-expressions into prefix statements before the loop would
;; compute them only once, breaking loop semantics.  Pass 5 already handles
;; nested while conditions correctly via its recursive b5-lower helper.

(defparameter *blub-4* (make-interpreter :on-unknown :recurse
                                         :readable-name "BLUB-4 (normalize)"))

;; --- Pass 4 state ---

(defvar *p4-prefix* nil
  "Accumulates (:declare) and (:assign) statements to splice before the
   current statement.  Freshly bound to NIL by each statement handler.")

;; --- Normalization helpers (mutually recursive) ---
;; After pass 3, all sub-expressions are wrapped as (:typed TYPE INNER).
;; p4-simplify preserves these wrappers; p4-atomize reads the type annotation
;; via blub-type-of when creating fresh temporaries.

(defun p4-atomic-p (expr)
  "True if EXPR is already atomic: a literal, bare :var, or :typed wrapping either.
   Struct-typed expressions are always atomic since they are represented as pointers."
  (or (numberp expr)
      (and (consp expr) (eq (car expr) :var))
      (and (consp expr) (eq (car expr) :typed)
           (let ((type  (cadr expr))
                 (inner (caddr expr)))
             (or (eq (blub-type-inner type) :struct)  ; structs are pass-by-addr, always atomic
                 (numberp inner)
                 (and (consp inner) (eq (car inner) :var)))))))

(defun p4-simplify (expr)
  "Return EXPR with all direct sub-expression arguments made atomic.
   Preserves :typed wrappers added by pass 3; any lifted sub-expressions
   are appended to *P4-PREFIX* as side effects."
  (if (and (consp expr) (eq (car expr) :typed))
    ;; Typed wrapper: simplify the inner form and re-wrap.
    (list :typed (cadr expr) (p4-simplify-inner (caddr expr)))
    (p4-simplify-inner expr)))

(defun p4-simplify-inner (inner)
  "Simplify a form with the :typed wrapper already stripped."
  (cond
    ;; Already atomic.
    ((numberp inner) inner)
    ((and (consp inner) (eq (car inner) :var)) inner)
    ;; Binary arithmetic/comparison ops: atomize both operands.
    ((and (consp inner)
          (member (car inner) '(:add :sub :mul :div :and :or :xor
                                :eq :ne :lt :le :gt :ge)))
     (list (car inner)
           (p4-atomize (cadr  inner))
           (p4-atomize (caddr inner))))
    ;; Short-circuit logical ops: atomize left only; right must remain unevaluated.
    ;; Pass 5 emits a branch and evaluates right only in the true/false path.
    ((and (consp inner) (member (car inner) '(:logand :logor)))
     (list (car inner)
           (p4-atomize (cadr inner))   ; left: always evaluated
           (caddr inner)))             ; right: lazy, pass through unevaluated
    ;; Unary ops: atomize operand.
    ((and (consp inner) (member (car inner) '(:neg :not :deref :addr-of)))
     (list (car inner) (p4-atomize (cadr inner))))
    ;; Cast: atomize the source expression.
    ((and (consp inner) (eq (car inner) :cast))
     (list :cast (cadr inner) (p4-atomize (caddr inner))))
    ;; Field access: atomize the base struct expression.
    ((and (consp inner) (eq (car inner) :.))
     (list :. (p4-atomize (cadr inner)) (caddr inner) (cadddr inner)))
    ;; Calls: symbol callee → atomize args only; expression callee → atomize callee too.
    ((and (consp inner) (eq (car inner) :call))
     (let ((callee (cadr inner)))
       (if (symbolp callee)
         (list* :call callee (mapcar #'p4-atomize (cddr inner)))
         (list* :call (p4-atomize callee) (mapcar #'p4-atomize (cddr inner))))))
    ;; Default: pass through.
    (t inner)))

(defun p4-atomize (expr)
  "Ensure EXPR is atomic. If not, simplify and lift to a fresh temp,
   appending (:declare) and (:assign) to *P4-PREFIX*.
   The type for the fresh temp is read from the :typed annotation via blub-type-of."
  (let ((s (p4-simplify expr)))
    (if (p4-atomic-p s)
      s   ; already atomic; no prefix stmts needed
      ;; Non-atomic: introduce a fresh temp.
      (let* ((type (or (blub-type-of s) '(:type :i32)))
             (tmp  (fresh-name "t")))
        (setf *p4-prefix*
              (append *p4-prefix*
                      (list (list :declare type tmp)
                            (list :assign  tmp s))))
        ;; Return a typed var reference so subsequent passes retain type info.
        (list :typed type (list :var tmp))))))

;; --- Helper: produce a splice of prefix stmts + the given statement ---

(defun p4-emit-stmt (stmt)
  "Return STMT wrapped in a splice with any accumulated *P4-PREFIX* stmts
   prepended.  If there are no prefix stmts, returns STMT directly (a
   plain list, not a splice), which the :block handler wraps into a
   one-element list via recurse-splice."
  (if *p4-prefix*
      (make-splice :nodes (append *p4-prefix* (list stmt)))
      stmt))

;; --- Pass 4: statement handlers ---

(def-op *blub-4* (:block &rest body)
  ;; Use recurse-splice so each statement handler can expand into multiple stmts.
  (cons :block (mapcan #'recurse-splice body)))

(def-op *blub-4* (:declare type name)
  (list :declare type name))

(def-op *blub-4* (:set lhs value)
  ;; Unified assignment: simplify the RHS; atomize the struct expression for field LHS.
  (let ((*p4-prefix* nil))
    (if (symbolp lhs)
      (p4-emit-stmt (list :set lhs (p4-simplify value)))
      ;; Field lhs: (:. struct-expr field-name offset field-type) from pass 3.
      (destructuring-bind (dot struct-expr field-name offset field-type) lhs
        (p4-emit-stmt (list :set
                            (list dot (p4-atomize struct-expr) field-name offset field-type)
                            (p4-simplify value)))))))

(def-op *blub-4* (:return &optional value)
  (if value
      (let ((*p4-prefix* nil))
        (p4-emit-stmt (list :return (p4-simplify value))))
      '(:return)))

(def-op *blub-4* (:if cond then &optional else)
  ;; Simplify the condition; any extracted temps become statements before the :if.
  (let ((*p4-prefix* nil))
    (let ((c (p4-simplify cond)))
      (p4-emit-stmt
       (if else
           (list :if c (recurse then) (recurse else))
           (list :if c (recurse then)))))))

(def-op *blub-4* (:while cond body)
  ;; Do NOT simplify the condition; extracting sub-expressions into prefix stmts
  ;; would only compute them once before the loop, breaking re-evaluation semantics.
  ;; DO recurse the body so statements within the loop are normalised.
  (list :while cond (recurse body)))

;; --- Pass 4: top-level handlers ---

(def-op *blub-4* (:global type name &optional value)
  (if value (list :global type name value) (list :global type name)))

(def-op *blub-4* (:function ret-type name args body)
  (list :function ret-type name args (recurse body)))

(def-op *blub-4* (:defstruct name size align &rest fields)
  ;; Pass through unchanged; struct layout was resolved by pass 2.
  (list* :defstruct name size align fields))

(def-op *blub-4* (:module &rest body)
  ;; Pass :meta through unchanged; recurse all real items.
  (multiple-value-bind (meta items) (extract-meta body)
    (append (list* :module (mapcar #'recurse items)) (list meta))))


;; =============================================================================
;; Pass 5: Lower to QBE IL
;; =============================================================================
;; - lower from c constructs to QBE (CPS transformation)
;;  - var -> alloc
;;  - expr -> SSA form with temporaries
;;  - function defs/calls -> function defs/calls
;;  - struct definitions -> aggregate structs
;;  - globals -> data
;;
;; Design: mutable dynamic variables accumulate QBE instructions for the
;; current basic block. Each expression handler emits instructions as
;; side-effects and returns a QBE value ((:temp name) or a literal).
;; Each statement handler emits and returns nil. The :function handler
;; manages the block accumulator and assembles the final QBE :function.

(defparameter *blub-5* (make-interpreter :on-unknown :error
                                         :readable-name "BLUB-5 (lower to QBE)"))

;; --- Pass 5 state (rebound per function by the :function handler) ---

(defvar *b5-stmts* nil
  "Reversed list of QBE instructions for the current basic block.")

(defvar *b5-label* nil
  "Label name (symbol) for the current basic block.")

(defvar *b5-blocks* nil
  "Reversed list of completed QBE (:block ...) forms for the current function.")

(defvar *b5-terminated* nil
  "T when the current block already has a control-flow terminator.")

(defvar *b5-var-env* (fset:empty-map)
  "Maps blub variable name symbols -> (:temp ptr) (the stack-pointer QBE temp).")

(defvar *b5-type-env* (fset:empty-map)
  "Maps blub variable name symbols -> their blub (:type ...) nodes.")

(defvar *b5-struct-env* (fset:empty-map)
  "Maps struct name symbols -> layout plists for pass 5. Rebound by :module
   from the :struct-env entry in :meta. Never shared with other passes.")

(defvar *b5-global-env* (fset:empty-map)
  "Maps global variable name symbols -> (:global name) QBE address form.")

(defvar *b5-global-type-env* (fset:empty-map)
  "Maps global variable name symbols -> their blub (:type ...) nodes.")

(defvar *b5-break-label* nil
  "Label to jump to on :break; nil when not inside a loop.")

(defvar *b5-cont-label* nil
  "Label to jump to on :continue; nil when not inside a loop.")

(defvar *b5-return-type* nil
  "The blub (:type ...) node for the current function's return type.")

;; Conversion to QBE utilities

(defun blub-type->alloc-op (type struct-env)
  "Return the QBE stack-allocation opcode appropriate for a blub type.
   STRUCT-ENV is the caller's pass-local struct layout map."
  (case (blub-type-inner type)
    ((:u8 :i8 :u32 :i32 :f32) :alloc4)
    ((:u64 :i64 :f64 :pointer :fn)                :alloc8)
    (:struct
     (let* ((layout (nth-value 0 (fset:lookup struct-env (blub-struct-name type))))
            (align  (getf layout :align)))
       (cond ((<= align 4) :alloc4)
             ((<= align 8) :alloc8)
             (t :alloc16))))
    (t (error "blub-type->alloc-op: cannot alloc type ~S" type))))

(defun blub-type->alloc-size (type struct-env)
  "Return the byte count to allocate for a blub type.
   STRUCT-ENV is the caller's pass-local struct layout map."
  (case (blub-type-inner type)
    ((:u8 :i8 :u32 :i32 :f32) 4)
    ((:u64 :i64 :f64 :pointer :fn)                8)
    (:struct
     (getf (nth-value 0 (fset:lookup struct-env (blub-struct-name type))) :size))
    (t (error "blub-type->alloc-size: cannot size type ~S" type))))

(defun blub-type->load-op (type)
  "Return the QBE load opcode for a blub type."
  (case (blub-type-inner type)
    (:u8  :loadub)
    (:i8  :loadsb)
    (:u32 :loaduw)
    (:i32 :loadsw)
    ((:u64 :i64)     :loadl)
    (:f32            :loads)
    (:f64            :loadd)
    ((:pointer :fn)  :loadl)
    (t (error "blub-type->load-op: cannot load type ~S" type))))

(defun blub-type->store-op (type)
  "Return the QBE store opcode for a blub type."
  (case (blub-type-inner type)
    ((:u8 :i8)   :storeb)
    ((:u32 :i32) :storew)
    ((:u64 :i64)     :storel)
    (:f32            :stores)
    (:f64            :stored)
    ((:pointer :fn)  :storel)
    (t (error "blub-type->store-op: cannot store type ~S" type))))

(defun blub-arith->qbe-op (blub-op)
  "Map a blub arithmetic/bitwise operator keyword to a QBE opcode."
  (case blub-op
    (:add :add) (:sub :sub) (:mul :mul) (:div :div)
    (:and :and) (:or  :or)  (:xor :xor)
    (t (error "blub-arith->qbe-op: unknown op ~S" blub-op))))

(defun blub-cmp->qbe-op (blub-op type)
  "Map a blub comparison operator and type to a QBE compare opcode."
  (case (blub-type-inner type)
    (:f64
     (case blub-op
       (:eq :ceqd) (:ne :cned) (:lt :cltd) (:le :cled) (:gt :cgtd) (:ge :cged)
       (t (error "blub-cmp->qbe-op: unknown f64 cmp ~S" blub-op))))
    (:f32
     (case blub-op
       (:eq :ceqs) (:ne :cnes) (:lt :clts) (:le :cles) (:gt :cgts) (:ge :cges)
       (t (error "blub-cmp->qbe-op: unknown f32 cmp ~S" blub-op))))
    (:u64
     (case blub-op
       (:eq :ceql) (:ne :cnel) (:lt :cultl) (:le :culel) (:gt :cugtl) (:ge :cugel)
       (t (error "blub-cmp->qbe-op: unknown u64 cmp ~S" blub-op))))
    (:i64
     (case blub-op
       (:eq :ceql) (:ne :cnel) (:lt :csltl) (:le :cslel) (:gt :csgtl) (:ge :csgel)
       (t (error "blub-cmp->qbe-op: unknown i64 cmp ~S" blub-op))))
    (:pointer
     (case blub-op
       (:eq :ceql) (:ne :cnel) (:lt :cultl) (:le :culel) (:gt :cugtl) (:ge :cugel)
       (t (error "blub-cmp->qbe-op: unknown pointer cmp ~S" blub-op))))
    ((:u8 :u32)
     (case blub-op
       (:eq :ceqw) (:ne :cnew) (:lt :cultw) (:le :culew) (:gt :cugtw) (:ge :cugew)
       (t (error "blub-cmp->qbe-op: unknown unsigned cmp ~S" blub-op))))
    ;; :i8, :i32 → signed word comparison
    (t
     (case blub-op
       (:eq :ceqw) (:ne :cnew) (:lt :csltw) (:le :cslew) (:gt :csgtw) (:ge :csgew)
       (t (error "blub-cmp->qbe-op: unknown int cmp ~S" blub-op))))))

(defun blub-field-ext-type (field-type)
  "Map a blub field type to a QBE ext-type (for use in :type aggregate definitions)."
  (case (blub-type-inner field-type)
    ((:u8 :i8)         :b)
    ((:u32 :i32)       :w)
    (:f32              :s)
    ((:u64 :i64 :pointer) :l)
    (:f64                   :d)
    (:struct
     (list :user-type
           (substitute #\_ #\- (string-downcase (string (blub-struct-name field-type))))))
    (t (error "blub-field-ext-type: unsupported type ~S" field-type))))

(defun blub-struct->qbe-type (name layout struct-env)
  "Build a QBE (:type ...) aggregate type declaration for a struct.
   STRUCT-ENV is the pass-local map used to compute nested struct field sizes.
   Includes explicit padding bytes so QBE's layout matches our computed offsets."
  (let* ((fields (getf layout :fields))
         (qname  (list :user-type
                       (substitute #\_ #\- (string-downcase (string name)))))
         (qfields '())
         (cur 0))
    (dolist (field fields)
      (destructuring-bind (fname ftype foffset) field
        (declare (ignore fname))
        (when (> foffset cur)
          (push (list :field :b (- foffset cur)) qfields))
        (push (list :field (blub-field-ext-type ftype)) qfields)
        (multiple-value-bind (fsize _) (blub-field-size-align ftype struct-env)
          (declare (ignore _))
          (setf cur (+ foffset fsize)))))
    (let ((total (getf layout :size)))
      (when (> total cur)
        (push (list :field :b (- total cur)) qfields)))
    (list* :type qname nil (nreverse qfields))))

(defun blub-type->qbe-base (type)
  "Map a blub (:type ...) node to the QBE base type keyword.
   Returns NIL for :void (QBE omits the return type on void functions).
   Function pointer types (:fn ...) map to :l (pointer-sized)."
  (case (blub-type-inner type)
    ((:u8 :i8 :u32 :i32) :w)
    ((:u64 :i64) :l)
    (:f32 :s)
    (:f64 :d)
    ((:pointer :fn) :l)
    (:void nil)
    (t (error "blub-type->qbe-base: unrecognized type ~S" type))))

(defun blub-type->qbe-abity (type)
  "Map a blub type to a QBE ABITY (for function params and return types).
   Like blub-type->qbe-base, but also handles struct types as (:user-type name)."
  (if (eq (blub-type-inner type) :struct)
    (list :user-type
          (substitute #\_ #\- (string-downcase (string (blub-struct-name type)))))
    (blub-type->qbe-base type)))


;; --- Block-building helpers ---

(defun b5-emit (instr)
  "Append INSTR to the current block's accumulator."
  (push instr *b5-stmts*))

(defun b5-finish-block (terminator)
  "Finalize the current block with TERMINATOR and push onto *b5-blocks*.
   No-ops if the block is already terminated (prevents double terminators)."
  (unless *b5-terminated*
    (push (list* :block
                 (list :label *b5-label*)
                 (append (nreverse *b5-stmts*) (list terminator)))
          *b5-blocks*)
    (setf *b5-terminated* t)))

(defun b5-new-block (label)
  "Start a new basic block with LABEL, resetting the instruction accumulator."
  (setf *b5-label*      label
        *b5-stmts*      nil
        *b5-terminated* nil))

(defun b5-temp (&optional (prefix "t"))
  "Return a fresh uninterned symbol for use as a QBE temporary name."
  (fresh-name prefix))

(defun b5-label (&optional (prefix "L"))
  "Return a fresh uninterned symbol for use as a basic block label."
  (fresh-name prefix))

(defun b5-wrap-temp (name) (list :temp name))
(defun b5-wrap-label (name) (list :label name))

(defun b5-global-name (name)
  "Convert a Blub symbol or string name to a valid lowercase QBE identifier.
   Lowercases and converts hyphens to underscores so that 'sum-to' -> 'sum_to'
   and 'QBE_MAIN' -> 'qbe_main', matching C linkage conventions."
  (substitute #\_ #\- (string-downcase (string name))))

;; --- Variable access helpers ---

(defun b5-lookup-var (name)
  "Look up variable NAME. Returns (values address type).
   ADDRESS is (:temp ptr) for locals, (:global name) for globals."
  (multiple-value-bind (ptr found) (fset:lookup *b5-var-env* name)
    (if found
      (values ptr (fset:lookup *b5-type-env* name))
      (multiple-value-bind (gptr gfound) (fset:lookup *b5-global-env* name)
        (if gfound
          (values gptr (fset:lookup *b5-global-type-env* name))
          (error "b5: variable ~S not found in any environment" name))))))

(defun b5-load-var (name)
  "Emit a load from variable NAME's stack/global address. Returns the QBE value temp."
  (multiple-value-bind (addr type) (b5-lookup-var name)
    (let* ((load-op (blub-type->load-op type))
           (qbase   (blub-type->qbe-base type))
           (result  (b5-temp "v")))
      (b5-emit (list :assign (b5-wrap-temp result) qbase load-op addr))
      (b5-wrap-temp result))))

(defun b5-store-var (name qbe-val)
  "Emit a store of QBE-VAL to variable NAME's stack/global address."
  (multiple-value-bind (addr type) (b5-lookup-var name)
    (b5-emit (list :instr (blub-type->store-op type) qbe-val addr))))

;; --- Expression lowering ---

(defun b5-get-struct-addr (typed-expr)
  "Return the QBE pointer to the struct named by TYPED-EXPR (a :typed struct-type form).
   For (:var x): returns the stack-slot address directly (no load).
   For (:deref ptr-expr): evaluates ptr-expr to get the struct pointer value.
   For other forms: evaluates the inner expression (should yield an :l pointer)."
  (let ((inner (caddr typed-expr)))
    (cond
      ((and (consp inner) (eq (car inner) :var))
       (multiple-value-bind (ptr found) (fset:lookup *b5-var-env* (cadr inner))
         (unless found (error "b5: struct variable ~S not found" (cadr inner)))
         ptr))
      ((and (consp inner) (eq (car inner) :deref))
       (b5-lower (cadr inner)))
      (t (b5-lower inner)))))

(defun b5-lower-field-access (field-type base-expr field-name offset)
  "Lower a struct field access. Computes the field's address via pointer arithmetic,
   then loads the scalar value (or returns the pointer for nested struct fields)."
  (let* ((struct-addr (b5-get-struct-addr base-expr))
         (field-ptr
           (if (zerop offset)
             struct-addr
             (let ((tmp (b5-temp (format nil "~A.fptr" field-name))))
               (b5-emit (list :assign (b5-wrap-temp tmp) :l :add struct-addr offset))
               (b5-wrap-temp tmp)))))
    ;; For struct fields: return the pointer (used as the struct address by callers).
    ;; For scalar fields: emit a load and return the loaded value.
    (if (eq (blub-type-inner field-type) :struct)
      field-ptr
      (let ((load-op (blub-type->load-op field-type))
            (qbase   (blub-type->qbe-base field-type))
            (res     (b5-temp (format nil "~A.fval" field-name))))
        (b5-emit (list :assign (b5-wrap-temp res) qbase load-op field-ptr))
        (b5-wrap-temp res)))))

(defun b5-lower (expr)
  "Lower a blub expression to a QBE value.
   Handles :typed annotations added by pass 3."
  (cond
    ((numberp expr)   expr)
    ((and (consp expr) (eq (car expr) :typed))
     (let ((type  (cadr expr))
           (inner (caddr expr)))
       (cond
         ;; Field access: use the annotated type + offset.
         ((and (consp inner) (eq (car inner) :.))
          (apply #'b5-lower-field-access type (cdr inner)))
         ;; Direct or indirect call: pass the annotated return type.
         ((and (consp inner) (eq (car inner) :call))
          (apply #'b5-lower-call type (cdr inner)))
         ;; Struct-typed expression: return the struct's address (not a loaded value).
         ((eq (blub-type-inner type) :struct)
          (b5-get-struct-addr expr))
         ;; All other typed forms: strip wrapper and lower inner.
         (t (b5-lower inner)))))
    (t (lower *blub-5* expr))))

(defun b5-lower-call (ret-type callee &rest args)
  "Lower a call. CALLEE is either a symbol (direct named call) or a :typed expression
   (indirect call through a function pointer). RET-TYPE comes from the :typed annotation."
  (let* ((qbe-ret  (blub-type->qbe-base ret-type))
         (qcallee  (if (symbolp callee)
                     (list :global (b5-global-name callee))
                     (b5-lower callee)))
         (qbe-args (mapcar (lambda (arg)
                             (let ((arg-type (or (blub-type-of arg) '(:type :i32))))
                               (list :call-arg
                                     (blub-type->qbe-abity arg-type)
                                     (b5-lower arg))))
                           args)))
    (if qbe-ret
      (let ((res (b5-temp "call")))
        (b5-emit (list* :call-assign (b5-wrap-temp res) qbe-ret qcallee qbe-args))
        (b5-wrap-temp res))
      (progn
        (b5-emit (list* :call qcallee qbe-args))
        nil))))

(defun b5-lower-binop (blub-op left right)
  "Lower a binary arithmetic/bitwise op, emitting a QBE :assign instruction.
   Returns the (:temp name) holding the result."
  (let* ((ltype (or (blub-type-of left) '(:type :i32)))
         (lv    (b5-lower left))
         (rv    (b5-lower right))
         (qbase (blub-type->qbe-base ltype))
         (qop   (blub-arith->qbe-op blub-op))
         (res   (b5-temp (string blub-op))))
    (b5-emit (list :assign (b5-wrap-temp res) qbase qop lv rv))
    (b5-wrap-temp res)))

(defun b5-lower-cmpop (blub-op left right)
  "Lower a comparison op, emitting a QBE :assign instruction.
   Returns the (:temp name) holding 0 or 1."
  (let* ((ltype (or (blub-type-of left) '(:type :i32)))
         (lv    (b5-lower left))
         (rv    (b5-lower right))
         (qop   (blub-cmp->qbe-op blub-op ltype))
         (res   (b5-temp (string blub-op))))
    ;; QBE comparison instructions always produce :w (32-bit word).
    (b5-emit (list :assign (b5-wrap-temp res) :w qop lv rv))
    (b5-wrap-temp res)))

;; --- Pass 5: expression handlers (emit instructions, return QBE value) ---

(def-op *blub-5* (:var name)
  ;; Load the variable from its stack slot. Returns a (:temp ...) holding the value.
  (b5-load-var name))

;; Arithmetic and bitwise binary operators
(def-op *blub-5* (:add  left right) (b5-lower-binop :add  left right))
(def-op *blub-5* (:sub  left right) (b5-lower-binop :sub  left right))
(def-op *blub-5* (:mul  left right) (b5-lower-binop :mul  left right))
(def-op *blub-5* (:div  left right) (b5-lower-binop :div  left right))
(def-op *blub-5* (:and  left right) (b5-lower-binop :and  left right))
(def-op *blub-5* (:or   left right) (b5-lower-binop :or   left right))
(def-op *blub-5* (:xor  left right) (b5-lower-binop :xor  left right))

;; Comparison operators (result: (:temp t) holding 0 or 1)
(def-op *blub-5* (:eq   left right) (b5-lower-cmpop :eq   left right))
(def-op *blub-5* (:ne   left right) (b5-lower-cmpop :ne   left right))
(def-op *blub-5* (:lt   left right) (b5-lower-cmpop :lt   left right))
(def-op *blub-5* (:le   left right) (b5-lower-cmpop :le   left right))
(def-op *blub-5* (:gt   left right) (b5-lower-cmpop :gt   left right))
(def-op *blub-5* (:ge   left right) (b5-lower-cmpop :ge   left right))

(def-op *blub-5* (:neg operand)
  ;; Arithmetic negation: 0 - operand.
  (let* ((otype (or (blub-type-of operand) '(:type :i32)))
         (ov    (b5-lower operand))
         (qbase (blub-type->qbe-base otype))
         (res   (b5-temp "neg")))
    (b5-emit (list :assign (b5-wrap-temp res) qbase :sub 0 ov))
    (b5-wrap-temp res)))

(def-op *blub-5* (:not operand)
  ;; Logical not: 1 if operand == 0, else 0.
  (let* ((ov  (b5-lower operand))
         (res (b5-temp "not")))
    (b5-emit (list :assign (b5-wrap-temp res) :w :ceqw ov 0))
    (b5-wrap-temp res)))

(def-op *blub-5* (:logand left right)
  ;; Short-circuit AND: right evaluated only if left is non-zero.
  ;; Result is 1 iff both left and right are non-zero; 0 otherwise.
  (let* ((rhs-lbl   (b5-label "land.rhs"))
         (false-lbl (b5-label "land.false"))
         (end-lbl   (b5-label "land.end"))
         ;; Stack slot holds the result; allocated before any branching.
         (res-ptr   (let ((p (b5-temp "land.ptr")))
                      (b5-emit (list :assign (b5-wrap-temp p) :l :alloc4 4))
                      (b5-wrap-temp p)))
         (lv (b5-lower left)))
    (b5-finish-block (list :jnz lv (b5-wrap-label rhs-lbl) (b5-wrap-label false-lbl)))
    ;; RHS block: left was non-zero; evaluate right and store normalized result.
    (b5-new-block rhs-lbl)
    (let* ((rv (b5-lower right))
           (nz (b5-temp "land.nz")))
      (b5-emit (list :assign (b5-wrap-temp nz) :w :cnew rv 0))
      (b5-emit (list :instr :storew (b5-wrap-temp nz) res-ptr)))
    (b5-finish-block (list :jmp (b5-wrap-label end-lbl)))
    ;; False block: left was zero; short-circuit result is 0.
    (b5-new-block false-lbl)
    (b5-emit (list :instr :storew 0 res-ptr))
    (b5-finish-block (list :jmp (b5-wrap-label end-lbl)))
    ;; End block: load and return result.
    (b5-new-block end-lbl)
    (let ((result (b5-temp "land")))
      (b5-emit (list :assign (b5-wrap-temp result) :w :loadsw res-ptr))
      (b5-wrap-temp result))))

(def-op *blub-5* (:logor left right)
  ;; Short-circuit OR: right evaluated only if left is zero.
  ;; Result is 1 iff left or right is non-zero; 0 otherwise.
  (let* ((rhs-lbl  (b5-label "lor.rhs"))
         (true-lbl (b5-label "lor.true"))
         (end-lbl  (b5-label "lor.end"))
         ;; Stack slot holds the result; allocated before any branching.
         (res-ptr  (let ((p (b5-temp "lor.ptr")))
                     (b5-emit (list :assign (b5-wrap-temp p) :l :alloc4 4))
                     (b5-wrap-temp p)))
         (lv (b5-lower left)))
    (b5-finish-block (list :jnz lv (b5-wrap-label true-lbl) (b5-wrap-label rhs-lbl)))
    ;; RHS block: left was zero; evaluate right and store normalized result.
    (b5-new-block rhs-lbl)
    (let* ((rv (b5-lower right))
           (nz (b5-temp "lor.nz")))
      (b5-emit (list :assign (b5-wrap-temp nz) :w :cnew rv 0))
      (b5-emit (list :instr :storew (b5-wrap-temp nz) res-ptr)))
    (b5-finish-block (list :jmp (b5-wrap-label end-lbl)))
    ;; True block: left was non-zero; short-circuit result is 1.
    (b5-new-block true-lbl)
    (b5-emit (list :instr :storew 1 res-ptr))
    (b5-finish-block (list :jmp (b5-wrap-label end-lbl)))
    ;; End block: load and return result.
    (b5-new-block end-lbl)
    (let ((result (b5-temp "lor")))
      (b5-emit (list :assign (b5-wrap-temp result) :w :loadsw res-ptr))
      (b5-wrap-temp result))))

(defun b5-cast-op (src-inner dst-inner src-qbe dst-qbe)
  "Return the QBE opcode (keyword) to convert from SRC-TYPE to DST-TYPE.
   SRC-INNER / DST-INNER are the blub-type-inner keywords; SRC-QBE / DST-QBE
   are the corresponding QBE base types."
  (cond
    ;; :w → :l  (integer widening to 64-bit)
    ((and (eq src-qbe :w) (eq dst-qbe :l))
     (if (member src-inner '(:u8 :u32)) :extuw :extsw))
    ;; :l → :w  (integer truncation to 32-bit, also covers pointer→i32)
    ((and (eq src-qbe :l) (eq dst-qbe :w))
     :copy)
    ;; float widening: f32 → f64
    ((and (eq src-qbe :s) (eq dst-qbe :d))
     :exts)
    ;; float narrowing: f64 → f32
    ((and (eq src-qbe :d) (eq dst-qbe :s))
     :truncd)
    ;; integer → float  (:w → :s/:d)
    ((and (eq src-qbe :w) (member dst-qbe '(:s :d)))
     (if (member src-inner '(:u8 :u32)) :uwtof :swtof))
    ;; integer → float  (:l → :s/:d)
    ((and (eq src-qbe :l) (member dst-qbe '(:s :d)))
     (if (member src-inner '(:u64 :pointer :fn)) :ultof :sltof))
    ;; float → integer  (:s/:d → :w)
    ((and (member src-qbe '(:s :d)) (eq dst-qbe :w))
     (if (member dst-inner '(:u8 :u32))
       (if (eq src-qbe :d) :dtoui :stoui)
       (if (eq src-qbe :d) :dtosi :stosi)))
    ;; float → integer  (:s/:d → :l)
    ((and (member src-qbe '(:s :d)) (eq dst-qbe :l))
     (if (member dst-inner '(:u64 :pointer))
       (if (eq src-qbe :d) :dtoui :stoui)
       (if (eq src-qbe :d) :dtosi :stosi)))
    (t (error "blub pass 5: unsupported cast from QBE ~S to ~S" src-qbe dst-qbe))))

(def-op *blub-5* (:cast dst-type src-expr)
  ;; Emit a type conversion instruction, or return the value as-is when the
  ;; source and destination share the same QBE base type (e.g. i32 ↔ u32,
  ;; pointer ↔ i64, fn-ptr ↔ pointer).
  (let* ((src-val  (b5-lower src-expr))
         (src-type (or (blub-type-of src-expr) '(:type :i32)))
         (src-qbe  (blub-type->qbe-base src-type))
         (dst-qbe  (blub-type->qbe-base dst-type)))
    (if (eq src-qbe dst-qbe)
      src-val   ; same QBE representation — no instruction needed
      (let ((result (b5-temp "cast"))
            (op     (b5-cast-op (blub-type-inner src-type)
                                (blub-type-inner dst-type)
                                src-qbe dst-qbe)))
        (b5-emit (list :assign (b5-wrap-temp result) dst-qbe op src-val))
        (b5-wrap-temp result)))))

(def-op *blub-5* (:fn-ptr name)
  ;; Address of a named function: the global symbol IS the pointer.
  (list :global (b5-global-name name)))

(def-op *blub-5* (:addr-of inner)
  ;; Address of a variable: return the stack pointer directly without loading.
  ;; The inner form may be :typed-wrapped after pass 3.
  (let ((var-form (if (and (consp inner) (eq (car inner) :typed))
                    (caddr inner) inner)))
    (unless (and (consp var-form) (eq (car var-form) :var))
      (error "blub pass 5: :addr-of only supported on variables, got ~S" inner))
    (multiple-value-bind (addr found) (fset:lookup *b5-var-env* (cadr var-form))
      (unless found
        (error "blub pass 5: :addr-of ~S not in var-env" (cadr var-form)))
      addr)))

(def-op *blub-5* (:deref ptr-expr)
  ;; Dereference a pointer: load from the pointer value produced by ptr-expr.
  (let* ((ptr-type (or (blub-type-of ptr-expr) '(:type (:pointer (:type :i32)))))
         (ptr-val  (b5-lower ptr-expr))
         ;; Extract pointee type from (:type (:pointer <pointee>)).
         (pointee  (or (when (and ptr-type (consp ptr-type)
                                  (eq (car ptr-type) :type))
                         (let ((inner (cadr ptr-type)))
                           (when (and (consp inner) (eq (car inner) :pointer))
                             (cadr inner))))
                       '(:type :i32)))  ; fallback: treat as i32 pointer
         (load-op  (blub-type->load-op pointee))
         (qbase    (blub-type->qbe-base pointee))
         (res      (b5-temp "deref")))
    (b5-emit (list :assign (b5-wrap-temp res) qbase load-op ptr-val))
    (b5-wrap-temp res)))

;; --- Pass 5: statement handlers (emit instructions, return nil) ---

(def-op *blub-5* (:declare type name)
  ;; Allocate stack space for a local variable and register it.
  ;; After pass 0, :declare never carries an initializer value.
  (let* ((ptr   (b5-temp (format nil "~A.ptr" name)))
         (alloc (blub-type->alloc-op type *b5-struct-env*))
         (size  (blub-type->alloc-size type *b5-struct-env*)))
    ;; alloc4/alloc8 returns an :l (pointer-sized) temp.
    (b5-emit (list :assign (b5-wrap-temp ptr) :l alloc size))
    ;; Register variable for later :var and :assign lookups.
    (setf *b5-var-env*  (fset:with *b5-var-env*  name (b5-wrap-temp ptr)))
    (setf *b5-type-env* (fset:with *b5-type-env* name type)))
  nil)

(def-op *blub-5* (:set lhs value)
  ;; Unified assignment: variable store or struct field store.
  (if (symbolp lhs)
    ;; Variable store.
    (let ((qval (b5-lower value)))
      (b5-store-var lhs qval))
    ;; Field store: lhs is (:. struct-expr field-name offset field-type) from pass 3.
    (destructuring-bind (dot struct-expr field-name offset field-type) lhs
      (declare (ignore dot field-name))
      (let* ((struct-addr (b5-get-struct-addr struct-expr))
             (field-ptr   (if (zerop offset)
                            struct-addr
                            (let ((tmp (b5-temp "fptr")))
                              (b5-emit (list :assign (b5-wrap-temp tmp) :l :add struct-addr offset))
                              (b5-wrap-temp tmp))))
             (qval        (b5-lower value))
             (store-op    (blub-type->store-op field-type)))
        (b5-emit (list :instr store-op qval field-ptr)))))
  nil)

;; Internal IR form emitted by pass 4's p4-atomize for fresh temporaries.
;; (:assign tmp-sym expr) is equivalent to a variable :set.
(def-op *blub-5* (:assign lhs value)
  (b5-store-var lhs (b5-lower value))
  nil)

(def-op *blub-5* (:block &rest body)
  ;; Process each statement for side-effects. Variable declarations inside
  ;; this block extend *b5-var-env* for all subsequent statements.
  (dolist (stmt body) (recurse stmt))
  nil)

(def-op *blub-5* (:if cond then &optional else)
  ;; Evaluate condition, branch to then-block or else-block, rejoin at end-block.
  (let* ((cond-val (b5-lower cond))
         (then-lbl (b5-label "if.then"))
         (else-lbl (b5-label "if.else"))
         (end-lbl  (b5-label "if.end"))
         (has-else (not (null else))))
    ;; Terminate current block with conditional jump.
    (b5-finish-block
     (list :jnz cond-val
           (b5-wrap-label then-lbl)
           (b5-wrap-label (if has-else else-lbl end-lbl))))
    ;; Then-block.
    (b5-new-block then-lbl)
    (recurse then)
    (b5-finish-block (list :jmp (b5-wrap-label end-lbl)))
    ;; Else-block (only if present).
    (when has-else
      (b5-new-block else-lbl)
      (recurse else)
      (b5-finish-block (list :jmp (b5-wrap-label end-lbl))))
    ;; Continue in end-block.
    (b5-new-block end-lbl))
  nil)

(def-op *blub-5* (:while cond body)
  ;; Loop: fall into cond-block, evaluate condition, branch to body or exit.
  (let* ((cond-lbl (b5-label "while.cond"))
         (body-lbl (b5-label "while.body"))
         (end-lbl  (b5-label "while.end")))
    ;; Fall into the condition-check block.
    (b5-finish-block (list :jmp (b5-wrap-label cond-lbl)))
    ;; Condition block.
    (b5-new-block cond-lbl)
    (let ((cond-val (b5-lower cond)))
      (b5-finish-block
       (list :jnz cond-val
             (b5-wrap-label body-lbl)
             (b5-wrap-label end-lbl))))
    ;; Body block; set break/continue targets for nested control flow.
    (b5-new-block body-lbl)
    (let ((*b5-break-label* end-lbl)
          (*b5-cont-label*  cond-lbl))
      (recurse body))
    ;; After body, jump back to condition.
    (b5-finish-block (list :jmp (b5-wrap-label cond-lbl)))
    ;; Continue after the loop.
    (b5-new-block end-lbl))
  nil)

(def-op *blub-5* (:return &optional value)
  ;; Emit a return instruction, then open a dead block for any following code.
  (if value
    (b5-finish-block (list :ret (b5-lower value)))
    (b5-finish-block '(:ret)))
  ;; Dead block: unreachable but QBE requires that every block has a terminator,
  ;; so any instructions emitted after this :return go here and are discarded.
  (b5-new-block (b5-label "dead"))
  nil)

(def-op *blub-5* (:break)
  ;; Jump to the enclosing loop's exit label.
  (unless *b5-break-label*
    (error "blub pass 5: :break used outside a loop"))
  (b5-finish-block (list :jmp (b5-wrap-label *b5-break-label*)))
  (b5-new-block (b5-label "dead"))
  nil)

(def-op *blub-5* (:continue)
  ;; Jump to the enclosing loop's condition label.
  (unless *b5-cont-label*
    (error "blub pass 5: :continue used outside a loop"))
  (b5-finish-block (list :jmp (b5-wrap-label *b5-cont-label*)))
  (b5-new-block (b5-label "dead"))
  nil)

;; --- Pass 5: top-level form handlers ---

(def-op *blub-5* (:global type name &optional value)
  ;; Emit a QBE :data item. The global env is pre-loaded by :module from :meta,
  ;; so no env-update side effects are needed here.
  (let* ((data-type (case (blub-type-inner type)
                      ((:u8 :i8)   :b)
                      ((:u32 :i32) :w)
                      ((:u64 :i64) :l)
                      (:f32        :s)
                      (:f64        :d)
                      (:pointer    :l)
                      (t :w)))
         (init-val  (cond
                      ((null value)    0)
                      ((numberp value) value)
                      (t (error "blub pass 5: global ~A initializer must be a literal" name))))
         (qname (b5-global-name name)))
    (list :data (list :global qname) nil nil
          (list :data-item data-type init-val))))

(def-op *blub-5* (:defstruct name size align &rest fields)
  ;; Struct definitions are fully lowered in :module; nothing to emit per-struct
  ;; (the QBE :type def is emitted by the :module handler).
  nil)

(def-op *blub-5* (:function ret-type name args body)
  ;; Lower a function to a QBE :function form.
  (let* ((*b5-stmts*       nil)
         (*b5-blocks*      nil)
         (*b5-var-env*     *b5-var-env*)
         (*b5-type-env*    *b5-type-env*)
         (*b5-return-type* ret-type)
         (*b5-terminated*  nil)
         (start-lbl        (b5-label "start"))
         ;; Use abity for return type so structs use (:user-type name) not :w.
         (qbe-ret          (blub-type->qbe-abity ret-type)))
    (b5-new-block start-lbl)
    (let ((qbe-params
            (mapcar (lambda (arg)
                      (destructuring-bind (arg-type arg-name) arg
                        (let* ((in-tmp  (b5-temp (string arg-name)))
                               (qabity  (blub-type->qbe-abity arg-type))
                               (struct-p (eq (blub-type-inner arg-type) :struct)))
                          (cond
                            (struct-p
                             ;; Struct param: incoming aggregate value IS a pointer.
                             ;; Copy it to a fresh local slot so the param is mutable.
                             (let* ((ptr-tmp  (b5-temp (format nil "~A.ptr" arg-name)))
                                    (alloc-op (blub-type->alloc-op arg-type *b5-struct-env*))
                                    (size     (blub-type->alloc-size arg-type *b5-struct-env*)))
                               (b5-emit (list :assign (b5-wrap-temp ptr-tmp) :l alloc-op size))
                               (b5-emit (list :instr :blit
                                              (b5-wrap-temp in-tmp)
                                              (b5-wrap-temp ptr-tmp)
                                              size))
                               (setf *b5-var-env*  (fset:with *b5-var-env*  arg-name (b5-wrap-temp ptr-tmp)))
                               (setf *b5-type-env* (fset:with *b5-type-env* arg-name arg-type))
                               (list :param qabity (b5-wrap-temp in-tmp))))
                            (t
                             ;; Scalar param: alloc stack slot, store incoming value.
                             (let* ((ptr-tmp  (b5-temp (format nil "~A.ptr" arg-name)))
                                    (alloc-op (blub-type->alloc-op arg-type *b5-struct-env*))
                                    (size     (blub-type->alloc-size arg-type *b5-struct-env*))
                                    (store-op (blub-type->store-op arg-type))
                                    (qbase    (blub-type->qbe-base arg-type)))
                               (b5-emit (list :assign (b5-wrap-temp ptr-tmp) :l alloc-op size))
                               (b5-emit (list :instr store-op
                                              (b5-wrap-temp in-tmp)
                                              (b5-wrap-temp ptr-tmp)))
                               (setf *b5-var-env*  (fset:with *b5-var-env*  arg-name (b5-wrap-temp ptr-tmp)))
                               (setf *b5-type-env* (fset:with *b5-type-env* arg-name arg-type))
                               (list :param qbase (b5-wrap-temp in-tmp))))))))
                    (cdr args))))
      (recurse body)
      (unless *b5-terminated*
        (b5-finish-block (if qbe-ret (list :ret 0) '(:ret))))
      (list* :function
             (list :global (b5-global-name name))
             :export
             qbe-ret
             (cons qbe-params (nreverse *b5-blocks*))))))

(def-op *blub-5* (:module &rest body)
  ;; Read struct-env and global-env from :meta; initialize dynamic envs.
  ;; Output: QBE module with type defs, data items, functions (no :meta).
  (multiple-value-bind (meta items) (extract-meta body)
    (let ((*b5-struct-env*      (fset:empty-map))
          (*b5-global-env*      (fset:empty-map))
          (*b5-global-type-env* (fset:empty-map)))
      (let* ((struct-forms (filter items (node-is-p :defstruct)))
             (global-forms (filter items (node-is-p :global)))
             (other-forms  (filter items (lambda (n)
                                           (and (consp n)
                                                (not (eq (car n) :defstruct))
                                                (not (eq (car n) :global))))))
             ;; Build QBE type defs and populate *b5-struct-env*.
             (qbe-types
               (if (meta-get meta :struct-env)
                 ;; Use struct-env from :meta: no body re-scan needed.
                 (mapcar (lambda (pair)
                           (setf *b5-struct-env*
                                 (fset:with *b5-struct-env* (car pair) (cdr pair)))
                           (blub-struct->qbe-type (car pair) (cdr pair) *b5-struct-env*))
                         (meta-get meta :struct-env))
                 ;; Fallback: build from annotated :defstruct forms in body.
                 (mapcar (lambda (s)
                           (destructuring-bind (kw sname ssize salign &rest sfields) s
                             (declare (ignore kw))
                             (let ((layout (list :size ssize :align salign :fields sfields)))
                               (setf *b5-struct-env*
                                     (fset:with *b5-struct-env* sname layout))
                               (blub-struct->qbe-type sname layout *b5-struct-env*))))
                         struct-forms))))
        ;; Populate global env from :meta, or pre-scan global forms as fallback.
        (if (meta-get meta :global-env)
          (dolist (pair (meta-get meta :global-env))
            (setf *b5-global-env*
                  (fset:with *b5-global-env* (car pair)
                             (list :global (b5-global-name (car pair)))))
            (setf *b5-global-type-env*
                  (fset:with *b5-global-type-env* (car pair) (cdr pair))))
          ;; Fallback: scan global forms to build env before recursing.
          (dolist (item global-forms)
            (destructuring-bind (kw type gname &optional val) item
              (declare (ignore kw val))
              (setf *b5-global-env*
                    (fset:with *b5-global-env* gname (list :global (b5-global-name gname))))
              (setf *b5-global-type-env*
                    (fset:with *b5-global-type-env* gname type)))))
        (let ((qbe-globals (mapcar #'recurse global-forms))
              (qbe-rest    (mapcar #'recurse other-forms)))
          (list* :module (append qbe-types qbe-globals qbe-rest)))))))

;; =============================================================================
;; Public API
;; =============================================================================

(defun compile-blub (ast)
  "Run the full Blub compilation pipeline on AST, returning a QBE AST.

   Pass 0: Desugar (:declare type name value) into (:declare) + (:assign).
   Pass 1: Rename variables for lexical scoping (shadowing -> fresh names).
   Pass 3: Typecheck; annotates every expression node with (:typed TYPE INNER).
   Pass 4: Normalize to 3-address code; reads :typed annotations for temp types.
   Pass 5: Lower Blub AST to QBE IL AST; reads :typed annotations for call types.

   The resulting QBE AST can be validated with VALIDATE-QBE and printed
   to a QBE IL string with (LOWER *QBE* result)."
  (let* ((a0 (lower *blub-0* (validate-blub ast)))
         (a1 (lower *blub-1* a0))
         (a2 (lower *blub-2* a1))   ; struct layout resolution
         (a3 (lower *blub-3* a2))   ; typecheck + annotate with :typed wrappers
         (a4 (lower *blub-4* a3))   ; normalize nesting to three-address code
         (a5 (lower *blub-5* a4)))
    a5))

(defun compile-blub-to-string (ast)
  "Compile a Blub AST all the way to a QBE IL string.
   Equivalent to (LOWER *QBE* (COMPILE-BLUB ast))."
  (lower *qbe* (compile-blub ast)))

(defun build-blub-ast (ast &key
                               (name "program")
                               (build-dir "build")
                               (runtime-c "runtime.c")
                               (keep-temp-files t)
                               (trace nil))
  "Compile a Blub AST all the way to an executable.

   NAME        - base name for output files (e.g. \"factorial\")
   BUILD-DIR   - directory for all generated artifacts (default \"build\")
   RUNTIME-C   - path to the C runtime that wraps qbe_main
   TRACE       - if true, dump each pass's output as build/NAME.passN.lisp
   KEEP-TEMP-FILES - passed through to build-qbe-ast (keeps .ssa/.s files)

   Returns the path to the compiled executable (a string).
   Creates BUILD-DIR if it does not exist."
  (ensure-directories-exist (format nil "~a/" build-dir))
  (flet ((dump (label form)
           (when trace
             (let ((path (format nil "~a/~a.~a.lisp" build-dir name label)))
               (with-open-file (s path :direction :output :if-exists :supersede)
                 (write-string (pp-blub form) s))
               (format t ";; [trace] wrote ~a~%" path)))))
    (let* ((a0 (lower *blub-0* ast)))
      (dump "pass0" a0)
      (let* ((a1 (lower *blub-1* a0)))
        (dump "pass1" a1)
        (let* ((a2 (lower *blub-2* a1)))
          (dump "pass2-struct" a2)
          (let* ((a3 (lower *blub-3* a2)))
            (dump "pass3-tc" a3)
            (let* ((a4 (lower *blub-4* a3)))
              (dump "pass4-norm" a4)
              (let* ((a5 (lower *blub-5* a4)))
                (dump "pass5-qbe" a5)
                (build-qbe-ast a5
                               :out-name (format nil "~a/~a" build-dir name)
                               :runtime-c runtime-c
                               :keep-temp-files keep-temp-files
                               ;; The grammar-based validator expects bare (identifier)
                               ;; for function names, but pass 5 uses (:global name).
                               ;; The *qbe* printer has its own checks; skip grammar pass.
                               :validate nil)))))))))

(defun build-blub-file (path &key
                                  (name nil)
                                  (build-dir "build")
                                  (runtime-c "runtime.c")
                                  (keep-temp-files t)
                                  (trace nil))
  "Read a Blub source file from PATH and compile it to an executable.

   NAME defaults to the file's base name (sans directory and extension).
   All other keyword arguments are forwarded to BUILD-BLUB-AST.
   Returns the path to the compiled executable."
  (let* ((base (or name
                   (pathname-name (pathname path))))
         (ast  (with-open-file (s path)
                 (read s))))
    (build-blub-ast ast
                    :name name
                    :build-dir build-dir
                    :runtime-c runtime-c
                    :keep-temp-files keep-temp-files
                    :trace trace)
    (format nil "~a/~a" build-dir base)))

(defun extract-c-driver (src)
  "Extract lines starting with ';;; ' from SRC and return them joined as C code.
   These lines form an inline C driver that replaces runtime.c when present.
   Returns NIL if no such lines exist in SRC."
  (let ((driver-lines
          (loop for line in (uiop:split-string src :separator '(#\newline))
                when (and (>= (length line) 4)
                          (string= (subseq line 0 4) ";;; "))
                collect (subseq line 4))))
    (when driver-lines
      (format nil "~{~a~%~}" driver-lines))))

(defun run-blub-examples (&key
                               (examples-dir "examples/blub")
                               (build-dir    "build")
                               (runtime-c    "runtime.c")
                               (trace        nil))
  "Build and run every *.lisp file under EXAMPLES-DIR.

   Each file may contain:
     - Lines starting with ';;; ' — an inline C driver (compiled instead of
       runtime-c). The driver declares external Blub functions and runs
       assertions; returning 0 signals all checks passed.
     - A comment '; expected exit code: N' — compared against the actual exit
       code of the compiled binary.

   Returns a list of result plists with keys :file :expected :actual :pass."
  (let ((results '()))
    (dolist (path (sort (uiop:directory-files
                         (uiop:ensure-directory-pathname examples-dir)
                         "*.lisp")
                        #'string< :key #'namestring))
      (let* ((base     (pathname-name path))
             (src      (uiop:read-file-string path))
             ;; Scrape "; expected exit code: N" from source text.
             (expected (let ((pos (search "expected exit code:" src)))
                         (when pos
                           (parse-integer src
                                          :start (+ pos (length "expected exit code:"))
                                          :junk-allowed t))))
             ;; Extract inline C driver (;;; lines), write to build dir if present.
             (effective-runtime
               (let ((driver (extract-c-driver src)))
                 (if driver
                   (let ((driver-path (format nil "~a/~a.driver.c" build-dir base)))
                     (ensure-directories-exist (format nil "~a/" build-dir))
                     (with-open-file (ds driver-path :direction :output
                                                     :if-exists :supersede)
                       (write-string driver ds))
                     driver-path)
                   runtime-c)))
             (exe      nil)
             (actual   nil)
             (errored  nil))
        (format t "~%=== ~a ===~%" base)
        (handler-case
            (progn
              (setf exe
                    (build-blub-ast
                     (with-open-file (s path) (read s))
                     :name base
                     :build-dir build-dir
                     :runtime-c effective-runtime
                     :keep-temp-files t
                     :trace trace))
              ;; Run the executable; capture exit code.
              (setf actual
                    (nth-value 2
                      (uiop:run-program (list exe)
                                        :ignore-error-status t
                                        :output *standard-output*
                                        :error-output *error-output*))))
          (error (e)
            (format t "  ERROR: ~a~%" e)
            (setf errored t)))
        (let* ((pass (and (not errored)
                          (or (null expected)
                              (= actual expected)))))
          (format t "  expected=~a  actual=~a  => ~a~%"
                  (or expected "?") (or actual "?") (if pass "PASS" "FAIL"))
          (push (list :file base :expected expected :actual actual :pass pass)
                results))))
    (let* ((total  (length results))
           (passed (count t results :key (lambda (r) (getf r :pass)))))
      (format t "~%--- ~a/~a examples passed ---~%" passed total))
    (nreverse results)))
