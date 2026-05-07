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

;; -----------------------------------------------------------------------------
;; Blub pass 0: Desugaring
;; -----------------------------------------------------------------------------
;; - desugar combined declaration / assignment into separate declare and assign statements
;; - check that AST syntax is correct (like for function arguments and stuff)

(defparameter *blub-0* (make-interpreter :on-unknown :recurse))

(def-op *blub-0* (:declare type name &optional value)
  ;; TODO: If value is present, return a splice of (:declare type name) 
  ;; and (:assign name value). Otherwise just return the declaration.
  )

;; -----------------------------------------------------------------------------
;; Blub pass 1: Rename variables
;; -----------------------------------------------------------------------------
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

(defparameter *blub-1* (make-interpreter :on-unknown :recurse))

(define-pass-context *rename-env* :doc "Tracks variable renames for the blub-1 pass.")

(defun node-is-p (keyword)
  (lambda (node) (and (consp node) (eq (car node) keyword))))

(defun node-is-not-p (keyword)
  (lambda (node) (and (consp node) (not (eq (car node) keyword)))))

(defun filter (nodes predicate)
  (loop for node in nodes if (funcall predicate node) collect node))

(defun register-global (name)
  "Add NAME -> NAME to *rename-env*. Errors if NAME is already there."
  (when (nth-value 1 (env-lookup *rename-env* name))
    (error "Global variable ~A already declared." name))
  (env-bind *rename-env* name name)
  name)

(defun register-local (name)
  "Add NAME -> chosen-name to *rename-env*, freshening if NAME is
   already bound (shadowing). Returns the chosen name."
  (let* ((found     (nth-value 1 (env-lookup *rename-env* name)))
         (new-name  (if found (fresh-name (string name)) name)))
    (env-bind *rename-env* name new-name)
    new-name))

(defun lookup-or-error (name kind)
  "Look up NAME in *rename-env*. KIND is a string used in the error
   message (e.g. \"assigned\" or \"read\"). Returns the renamed symbol."
  (multiple-value-bind (mapped found) (env-lookup *rename-env* name)
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
  ;; Fresh dynamic binding initialized from outer scope, so changes don't leak out.
  (with-scope (*rename-env*)
    (cons :block (mapcar #'recurse body))))

(def-op *blub-1* (:function type name args block)
  (with-scope (*rename-env*)
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

(def-op *blub-1* (:module &rest body)
  ;; Fresh empty map at module scope. Globals are processed first so
  ;; their bindings are visible to all functions/blocks regardless of
  ;; textual order, then the rest of the module is renamed.
  (with-empty-scope (*rename-env*)
    (let ((globals (mapcar #'recurse (filter body (node-is-p :global))))
          (renamed (mapcar #'recurse (filter body (node-is-not-p :global)))))
      (cons :module (append globals renamed)))))

; (defparameter *blub-program* (car (read-example "blub/1-shadowing.lisp")))

; (validate-blub *blub-program*)
;
; (multiple-value-bind (body trace) (with-trace (lower *blub-1* *blub-program*))
;   (declare (ignore body))
;   (print-trace trace))
;
; (format t "~S~%" (lisp-to-string (lower *blub-1* *blub-program*)))

;; -----------------------------------------------------------------------------
;; Blub pass 2: Resolve functions and structs
;; -----------------------------------------------------------------------------
;; - collect all function/struct declarations, check that none conflict
;; - store function types and struct definitions in mapping
;; - compute sizes of structs

(defparameter *blub-2* (make-interpreter :on-unknown :passthrough))

(define-pass-context *signature-env* :doc "Stores function signatures and struct definitions.")

(def-op *blub-2* (:module &rest body)
  ;; TODO: collect all function/struct declarations, check that none conflict
  ;; store function types and struct definitions in mapping
  ;; compute sizes of structs
  )

;; -----------------------------------------------------------------------------
;; Blub pass 3: Typechecking
;; -----------------------------------------------------------------------------
;; - perform basic symbol->type typechecking
;; - typecheck standard operators, function calls, pointers, etc

(defparameter *blub-3* (make-interpreter :on-unknown :recurse))

(define-pass-context *type-env* :doc "Stores the lexical types of variables.")

(def-op *blub-3* (:declare type name &optional value)
  ;; TODO: register the variable's type in the environment. If value is provided,
  ;; ensure it matches the declared type.
  )

(def-op *blub-3* (:assign name value)
  ;; TODO: look up the variable type, typecheck the value, ensure they match.
  )

(def-op *blub-3* (:add left right)
  ;; TODO: typecheck binary operators to ensure operands are numeric types.
  )

(def-op *blub-3* (:call name &rest args)
  ;; TODO: look up function signature from signature-env, ensure arg count 
  ;; and types match.
  )

(def-op *blub-3* (:if cond then &optional else)
  ;; TODO: ensure condition typechecks to a boolean/int.
  )

;; -----------------------------------------------------------------------------
;; Blub pass 4: Simplify complex operands
;; -----------------------------------------------------------------------------
;; - extract nested expressions into temporaries so each statement does at most one operation.
;; - this prepares the AST for a simple lowering to QBE's SSA IL.

(defparameter *blub-4* (make-interpreter :on-unknown :recurse))

(def-op *blub-4* (:assign name value)
  ;; TODO: If value is complex, extract its components into temporaries
  ;; before performing the final assignment.
  )

(def-op *blub-4* (:call name &rest args)
  ;; TODO: If any args are complex, extract them into temporaries.
  )

(def-op *blub-4* (:if cond then &optional else)
  ;; TODO: If cond is complex, lift it out into a temporary before the if-statement.
  )

;; -----------------------------------------------------------------------------
;; Blub pass 5: Lower to QBE IL
;; -----------------------------------------------------------------------------
;; - lower from c constructs to QBE (CPS transformation)
;;  - var -> alloc
;;  - expr -> SSA form with temporaries
;;  - function defs/calls -> function defs/calls
;;  - struct definitions -> aggregate structs
;;  - globals -> data

(defparameter *blub-5* (make-interpreter :on-unknown :error))

(def-op *blub-5* (:module &rest body)
  ;; TODO: transform to QBE's (:module ...)
  )

(def-op *blub-5* (:global type name &optional value)
  ;; TODO: transform to QBE data items
  )

(def-op *blub-5* (:function type name args block)
  ;; TODO: transform to QBE function definition
  )

(def-op *blub-5* (:block &rest body)
  ;; TODO: compile control flow into linear QBE blocks with jumps/phis
  )

(def-op *blub-5* (:declare type name &optional value)
  ;; TODO: allocate stack memory for local variables
  )

(def-op *blub-5* (:assign name value)
  ;; TODO: emit QBE store instruction
  )

(def-op *blub-5* (:add left right)
  ;; TODO: emit QBE add instruction
  )
