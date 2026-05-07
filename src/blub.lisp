(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

(defparameter *blub-grammar*
  '((:module
     (repeat0 (option :function :global :block)))

    (:function
     :type (identifier) :args :block)

    (:args
     (repeat0 (list :type (identifier))))

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

(defun validate-blub (ast)
  (match-grammar ast :module *blub-grammar*)
  ast)

;; Blub (c-like language)
(defparameter *blub* (make-interpreter :on-unknown :passthrough))

;; Blub pretty printer
(defparameter *blub-print* (make-interpreter :on-unknown :passthrough))

;; How to store intermediate data to carry between passes? Like symbol table,
;; and the types of each variable. I guess the goal is to maybe try and keep
;; as much of the information local as possible.
;;
;; What if want to treat code differently depending on the outermost tag?

;; Language features
;;
;; declare variables with a type, variable assignment
;;  - int, float, double, boolean, pointer, arrays, byte (char)
;;  - everything stack allocated by default with alloc, read with load, set with store, since you need to be able to take their address
;;    - temporaries don't need an alloc, since you can't get a pointer to them
;; if, for, while, switch
;; functions
;; can declare global variables
;; standard arithmetic operators, comparisons, logic, bitwise
;; "block" (in curly braces)
;; function pointers
;; struct
;;  - note that in QBE, passing a struct using the aggregate type class is passing by value, while passing with type :l means passing the pointer itself (even though technically it is always a pointer)
;;  - so QBE deals with the C ABI for you when passing structs around
;; varargs
;; some way of accessing argv/argc
;; standard library functions for printing, etc
;;
;; Compiler passes (in no particular order)
;; - typecheck standard operators, function calls, pointers, etc
;; - resolve all struct definitions, determine total size, plus size and offset of each field
;; - cps transformation from nested statements into SSA
;;
;; Note: The exact grammar of the Blub AST is specified above in *BLUB-GRAMMAR*.
;; Use VALIDATE-BLUB to ensure an AST conforms to this grammar before processing.

;; Define individual passes within the blub language

;; Blub pass: desugaring
;; - desugar combined declaration / assignment
(defparameter *blub-0* (make-interpreter :on-unknown :recurse))

;;;; Blub pass: rename variables
;;
;; Lexical scoping rules:
;;
;; A variable can only be declared once within the global/block/function scope.
;; If a variable is re-declared, it becomes shadowed by the new name. Variables
;; can only be accessed after they have been declared.
;; 
;; Renaming algorithm:
;; Loop through all functions/block scopes
;; - On function declaration: add all parameters to new local lexical scope
;; - On block declaration: create new map for inner scope
;; - On variable declaration: if name already used, create fresh name

(defparameter *blub-1* (make-interpreter :on-unknown :recurse))

(defparameter *var-rename-map* (fset:empty-map))

(defun node-is-p (keyword)
  (lambda (node) (and (consp node) (eq (car node) keyword))))

(defun node-is-not-p (keyword)
  (lambda (node) (and (consp node) (not (eq (car node) keyword)))))

(defun filter (nodes predicate)
  (loop for node in nodes if (funcall predicate node) collect node))

(defun register-global (name)
  "Add NAME -> NAME to *var-rename-map*. Errors if NAME is already there."
  (when (nth-value 1 (fset:lookup *var-rename-map* name))
    (error "Global variable ~A already declared." name))
  (fset:includef *var-rename-map* name name)
  name)

(defun register-local (name)
  "Add NAME -> chosen-name to *var-rename-map*, freshening if NAME is
   already bound (shadowing). Returns the chosen name."
  (let* ((found     (nth-value 1 (fset:lookup *var-rename-map* name)))
         (new-name  (if found (fresh-name (string name)) name)))
    (fset:includef *var-rename-map* name new-name)
    new-name))

(defun lookup-or-error (name kind)
  "Look up NAME in *var-rename-map*. KIND is a string used in the error
   message (e.g. \"assigned\" or \"read\"). Returns the renamed symbol."
  (multiple-value-bind (mapped found) (fset:lookup *var-rename-map* name)
    (unless found (error "Variable ~A but not yet declared: ~A." kind name))
    mapped))

(def-op *blub-1* (:global type name &optional value)
  (register-global name)
  (let ((type-name (list type name)))
    (inherit-from type-name (expr))
    (list :global type name (recurse value))))

(def-op *blub-1* (:declare type name &optional value)
  ;; Recurse on VALUE *before* updating the map, so a self-referential
  ;; declaration like (:declare (int x) (:var x)) resolves :var x
  ;; against the OUTER scope's binding
  (let* ((lowered-value (recurse value))
         (new-name      (register-local name))
         (type-name     (list type new-name)))
    (inherit-from type-name (expr))
    (list :declare type new-name lowered-value)))

(def-op *blub-1* (:assign name value)
  (list :assign (lookup-or-error name "assigned") (recurse value)))

(def-op *blub-1* (:var name)
  (list :var (lookup-or-error name "read")))

(def-op *blub-1* (:block &rest body)
  ;; Fresh dynamic binding initialized from outer scope, so changes don't leak
  ;; out.
  ; (let ((*var-rename-map* *var-rename-map*))
    (cons :block (mapcar #'recurse body)))

(def-op *blub-1* (:function type name args block)
  ; (let ((*var-rename-map* *var-rename-map*))
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
      (list :function type name renamed-args (recurse block))))

(def-op *blub-1* (:module &rest body)
  ;; Fresh empty map at module scope. Globals are processed first so
  ;; their bindings are visible to all functions/blocks regardless of
  ;; textual order, then the rest of the module is renamed.
  (let* ((*var-rename-map* (fset:empty-map))
         (globals (mapcar #'recurse (filter body (node-is-p :global))))
         (renamed (mapcar #'recurse (filter body (node-is-not-p :global)))))
    (cons :module (append globals renamed))))

; (defparameter *blub-program* (car (read-example "blub/1-shadowing.lisp")))

; (validate-blub *blub-program*)
;
; (multiple-value-bind (body trace) (with-trace (lower *blub-1* *blub-program*))
;   (declare (ignore body))
;   (print-trace trace))
;
; (format t "~S~%" (lisp-to-string (lower *blub-1* *blub-program*)))

;; Blub pass: resolve functions/structs
(def-op *blub-1* (:module &body body)
    ;; collect all function/struct declarations, check that none conflict
    ;; store function types and struct definitions in mapping
    ;; compute sizes of structs
        )

;; Blub pass: simplify complex operands

;; Blub pass 3: perform basic symbol->type typechecking

;; Blub pass 4: lower from c constructs to QBE (CPS transformation)
;;  - var -> alloc
;;  - expr -> SSA form with temporaries
;;  - function defs/calls -> function defs/calls
;;  - struct definitions -> aggregate structs
;;  - globals -> data
