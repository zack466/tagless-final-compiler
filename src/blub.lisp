(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

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
;; Rough grammar:
;;
;; BLUB ::= MODULE [FUNCTION | GLOBAL | STRUCT_DEF]*
;;
;; GLOBAL ::= (GLOBAL_TYPE GLOBAL_NAME) [GLOBAL_VALUE]
;;
;; STRUCT_DEFS ::= STRUCT_NAME ((FIELD_TYPE FIELD_NAME)*)
;;
;; FUNCTION ::= OUT_TYPE FUNCTION_NAME ((ARG_TYPE ARG_NAMES)*) STATEMENT*


;; Define individual passes within the blub language

;; Blub pass: desugaring
;; - desugar combined declaration / assignment
;; - check that AST syntax is correct (like for function arguments and stuff)
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

(def-op *blub-1* (:global (type name) &optional value)
  ;; globals cannot have conflicting names.
  (when (nth-value 1 (fset:lookup *var-rename-map* name))
    (error "Global variable ~A already declared." name))
  (setf *var-rename-map* (fset:with *var-rename-map* name name))
  (list :global (list type name) (recurse value)))

(def-op *blub-1* (:declare (type name) &optional value)
  (multiple-value-bind (existing found) (fset:lookup *var-rename-map* name)
    (declare (ignore existing))
    (let ((new-name (if found (fresh-name (string name)) name))
          ;; Recurse on value BEFORE updating the map, so a self-referential
          ;; declaration like (:declare (int x) (:var x)) refers to the
          ;; outer x, not the one being declared.
          (lowered-value (recurse value)))
      (setf *var-rename-map* (fset:with *var-rename-map* name new-name))
      (list :declare (list type new-name) lowered-value))))

(def-op *blub-1* (:assign name value)
  (multiple-value-bind (mapped found) (fset:lookup *var-rename-map* name)
    (unless found (error "Variable not yet declared: ~A." name))
    (list :assign mapped (recurse value))))

(def-op *blub-1* (:var name)
  (multiple-value-bind (mapped found) (fset:lookup *var-rename-map* name)
    (unless found (error "Variable not yet declared: ~A." name))
    (list :var mapped)))

(def-op *blub-1* (:block &rest body)
  ;; Fresh dynamic binding initialized from outer scope; setf inside
  ;; only affects this binding, so changes don't leak out.
  (let ((*var-rename-map* *var-rename-map*))
    (cons :block (mapcar #'recurse body))))

(def-op *blub-1* (:function type name (&rest args) &rest body)
  (let ((*var-rename-map* *var-rename-map*))
    (loop for arg in args
          do (let ((arg-name (cadr arg)))
               (multiple-value-bind (existing found) (fset:lookup *var-rename-map* arg-name)
                (let ((new-name (if found (fresh-name (string arg-name)) arg-name)))
                  (setf *var-rename-map* (fset:with *var-rename-map* arg-name new-name))))))
    (list :function type name args (mapcar #'recurse body))))

(def-op *blub-1* (:module &rest body)
  (let* ((*var-rename-map* (fset:empty-map))
         (globals (mapcar #'recurse (filter body (node-is-p :global))))
         (renamed (mapcar #'recurse (filter body (node-is-not-p :global)))))
    (cons :module (append globals renamed))))

(defparameter *blub-program*
  '(:module
     (:global (int z) 10)
    (:function int qbe_main ((int x))
      (:declare (int y) 2)
      (:assign y (:add (:var x) (:var y))))
    (:block
      (:declare (int z) 1)
      (:declare (int y) 2)
      (:declare (int y) 3)
      (:assign y (:add (:var z) (:var y))))))

(lower *blub-1* *blub-program*)

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
