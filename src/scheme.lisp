(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

(defparameter *scheme-grammar*
  '((:module
     (repeat0 :define))

    (:define (identifier) :form)

    (:form
     (dispatch
      (option
       (literal)
       (identifier)
       :var
       ;; Unary
       :neg :not
       ;; Bitwise / arithmetic binary
       :add :sub :mul :div
       ;; Logical expressions
       :and :or :xor
       ;; Comparison
       :eq :ne :lt :le :gt :ge
       ;; Pairs
       :cons :car :cdr
       ;; Special forms
       :if :let :set :define :lambda
       ;; a list of forms is assumed to be a function application
       (list :form (repeat0 :form)))))

    (:body
     (repeat0 :form))

    (:args
     (repeat0 (identifier)))

    ;; special forms
    (:set (identifier) :form)
    (:let (list (identifier) :form) :body)
    (:lambda :args :body)
    (:if :form :form (maybe :form))  ; condition, then, optional else

    ;; Unary operators.
    (:neg      :form)
    (:not      :form)
    (:car      :form)
    (:cdr      :form)

    ;; Binary operations
    (:add      :form :form)
    (:sub      :form :form)
    (:mul      :form :form)
    (:div      :form :form)
    (:and      :form :form)
    (:or       :form :form)
    (:xor      :form :form)
    (:cons     :form :form)

    ;; Comparisons.
    (:eq       :form :form)
    (:ne       :form :form)
    (:lt       :form :form)
    (:le       :form :form)
    (:gt       :form :form)
    (:ge       :form :form)))

;; A quick scheme (compiles to blub)

(defparameter *scheme-0* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-0"))

; All values are 8 bytes: 1 byte of type tag followed by 7 bytes of data.
; - A cons cell is a pointer to two heap-allocated values (the car, followed by the cdr).
; - Integers, booleans, and floats are as expected.
; - A lambda is a pointer to a closure (a heap-allocated array of values) and a function pointer

; Core primitives:
; cons, car, cdr
; +, *, -, <, >, <=, >=, ==
; if, let, set, define, lambda

; Primitive functions roughly translate as:
;
; (cons x y) -> (call make-cons x y)
;
; val make_cons(val x, val y) {
;   val *ptr = malloc(2 * sizeof(val));
;   ptr[0] = x;
;   ptr[1] = y;
;   return cons_tag | ptr;
; }
;
; (car x) -> (call car x)
; val car(val x) {
;   return *(x & clear_tag);
; }
;
; (cdr x) -> (call cdr x)
; val cdr(val x) {
;   return *((x & clear_tag) + 1);
; }
;
; (binop x y) -> (call binop x y)
; val binop(val x, val y) {
;    if (!(numeric(x) && numeric(y))) {
;      error("error msg");
;    }
;    return type_tag | ((x & clear_tag) `binop` (y & clear_tag))
; }
;
; (let ((x1 y1) (x2 y2)) &body)
; ->
; (block
;   (declare x1 y1)
;   (declare x2 y2)
;   ...body)
;
; (let ((x1 y1) (x2 y2)) (lambda () (+ x1 x2)))
; ->
; (block
;   (declare x1 y1)
;   (declare x2 y2)
;   (declare env (make-closure x1 x2))
;   (lambda (env) (+ (get-env 0) (get-env 1))))
; ->
; (function lambda-body (env))
;   (+ (get-env 0) (get-env 1)))
; (block
;   (declare env (make-closure y1 y2))
;   (make-lambda env lambda-body))
;
;
; (lambda ()
;   (let ((x 0))
;     (lambda () (block (set x (+ x 1) x)))))
; ->
; (function lambda-0 (env)
;   (set-env 0 (+ (get-env 0) 1))
;   (get-env 0))
;
; (lambda () (block
;   (declare x 0)
;   (declare env (make-closure x))
;   (make-lambda env lamda-0)))
; ->
; (function lambda-0 (env)
;   (set-env 0 (+ (get-env 0) 1))
;   (get-env 0))
;
; (function lambda-1 ()
;   (declare x 0)
;   (declare env (make-closure x))
;   (make-lambda env lamda-0))
; 
; (make-lambda nil lambda-1)
;
; val make_closure(...args) {
;   val *vals = malloc(len(args) * sizeof(val));
;   memcpy(vals, args);
;   return env_tag | vals;
; }
;
; val get_env(env, n) {
;   return *(env & clear_tag + n);
; }
;
; val make_lambda(env, body) {
;   val *ptr = malloc(2 * sizeof(val));
;   ptr[0] = env;
;   ptr[1] = body;
;   return lambda_tag & ptr;
; }
;
; (f 1 2)
; ->
; (apply f 1 2)
;
; val apply(val f, ...args) {
;   if (is_lambda(f)) {
;     get_func(f)(get_closure(f), ...args);
;   }
;   error("cannot call non-function");
; }


;; Compiler passes:
;; 0 - desugaring
;; 1 - uniquify
;; 2 - compute closures for all lambdas
;; 3 - lift lambdas
;; 4 - flatten let
;; 5 - convert all function applications into apply
;; 6 - dispatch primitives as function calls
