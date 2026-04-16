(in-package #:tagless-compiler)

;; that "lowers" it and returns a new tree of symbols. If a symbol is not
;; provided with a lowering function, can either keep it the same, or throw an
;; error.

(defclass interpreter ()
  ((handlers :initform (make-hash-table :test #'eq) 
             :reader   handlers 
             :documentation "Hash table mapping keywords to handler functions.")))

(defun make-interpreter ()
  (make-instance 'interpreter))

; generic 'lower' function to enable recursion, dispatched on type of interpreter
(defgeneric lower (interpreter expression)
  (:documentation "Interprets the EXPRESSION using the rules defined in INTERPRETER."))

; "interprets" a cons cell by dispatching on its head symbol
(defmethod lower ((interp interpreter) (expr cons))
  "Interprets a list by looking up the operator keyword in the interpreter's table."
  (let ((op (first expr))
        (body (rest expr)))
    (if (keywordp op)
        (let ((handler (gethash op (handlers interp))))
          (if handler
              (funcall handler body)
              (error "No handler defined for operator ~S in interpreter ~S" op interp)))
        (error "Interpreter expects keyword operators, got ~S" op))))

; Fallback when interpreter is not defined on the symbol, just return unchanged
(defmethod lower ((interp interpreter) (expr t))
  expr)

; Defines a handler for a specific interpreter instance.
; Inside the body, 'lower' is rebound locally to ensure recursion uses 'interp'.
(defmacro def-op (interp keyword lambda-list &body body)
  `(setf (gethash ,keyword (handlers ,interp))
         (lambda (body-args)
           (destructuring-bind ,lambda-list body-args
             (flet ((lower (x) (lower ,interp x)))
               ,@body)))))


;; --- Interpreter 1: Arithmetic Evaluator ---
(defvar arith-eval (make-interpreter))

(def-op arith-eval :add (a b)
  (+ (lower a) (lower b)))

(def-op arith-eval :mul (a b)
  (* (lower a) (lower b)))

(def-op arith-eval :inc (a)
  (1+ (lower a)))

;; --- Interpreter 2: String Representation ---
(defvar string-repr (make-interpreter))

(def-op string-repr :add (a b)
  (format nil "(Add ~A ~A)" (lower a) (lower b)))

(def-op string-repr :mul (a b)
  (format nil "(Mul ~A ~A)" (lower a) (lower b)))

(def-op string-repr :inc (a)
  (format nil "(Inc ~A)" (lower a)))

;; --- Evaluation ---

;; Define a shared complex structure
(defvar *program* '(:mul (:add 1 2) (:inc 5)))

;; Run with Arithmetic Evaluator
(lower arith-eval *program*)
;; =>  (* (+ 1 2) (1+ 5))
;; =>  (* 3 6)
;; =>  18

;; Run with String Representation
(lower string-repr *program*)
;; => "(Mul (Add 1 2) (Inc 5))"

