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
       :if :let :set! :lambda :apply)))

    ;; ugly, but makes it easer to write compiler passes
    (:var (identifier))
    (:apply :form (repeat0 :form))

    ;; special forms
    (:set! (identifier) :form)
    (:let (list (repeat0 (list (identifier) :form))) (repeat0 :form))
    (:lambda (list (repeat0 (identifier))) (repeat0 :form))
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
;; 2 - convert assignments
;;     a. identify free variables
;;     b. identify variables that are written to (on lhs of set!)
;;     c. box variables in this intersection, convert reads into dereferences
;; 3 - closure conversion
;;     a. convert function references into closures
;; 4 - flatten let
;; 5 - convert all function applications into apply
;; 6 - dispatch primitives as function calls

;; Pass 0: desugaring
(defparameter *scheme-0* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-0"))

;; Pass 1: uniquify
(defparameter *scheme-1* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-1"))

(defparameter *s1-rename-env* (fset:empty-map) "Tracks variable renames")

(defun s1-register-global (name)
  "Add NAME -> NAME to *s1-rename-env*. Errors if NAME is already there."
  (when (nth-value 1 (fset:lookup *s1-rename-env* name))
    (error "Global variable ~A already declared." name))
  (setf *s1-rename-env* (fset:with *s1-rename-env* name name))
  name)

(defun s1-register-local (name)
  "Add NAME -> chosen-name to *s1-rename-env*, freshening if NAME is
   already bound (shadowing). Returns the chosen name."
  (let* ((found     (nth-value 1 (fset:lookup *s1-rename-env* name)))
         (new-name  (if found (fresh-name (string name)) name)))
    (setf *s1-rename-env* (fset:with *s1-rename-env* name new-name))
    new-name))

(defun s1-lookup-or-error (name kind)
  "Look up NAME in *s1-rename-env*. KIND is a string used in the error
   message (e.g. \"assigned\" or \"read\"). Returns the renamed symbol."
  (multiple-value-bind (mapped found) (fset:lookup *s1-rename-env* name)
    (unless found (error "Variable ~A but not yet declared: ~A." kind name))
    mapped))

(def-op *scheme-1* (:module &rest body)
  (let ((*s1-rename-env* (fset:empty-map)))
    (append (list :module) (mapcan #'recurse-splice body))))

(def-op *scheme-1* (:define name form)
  (s1-register-global name)
  (list :define name (recurse form)))

(def-op *scheme-1* (:let bindings &rest body)
  ;; lexical scope boundary
  (let ((*s1-rename-env* *s1-rename-env*))
    ;; For each declared variable, declare as a local
    (let ((bindings* (mapcar
                       (lambda (binding)
                         (let* ((name (s1-register-local (car binding)))
                                (value (recurse (cadr binding))))
                           (list name value)))
                          bindings)))
      ;; Recurse into the body
      (append (list :let bindings*)
              (mapcar #'recurse-splice body)))))

(def-op *scheme-1* (:lambda params &rest body)
  ;; lexical scope boundary
  (let ((*s1-rename-env* *s1-rename-env*))
    ;; For each argument, declare as a local
    (let ((params* (mapcar #'s1-register-local params)))
      ;; Recurse into the body
      (append (list :lambda params*)
              (mapcar #'recurse-splice body)))))

(def-op *scheme-1* (:var name)
  ;; lexical scope boundary
  (list :var (s1-lookup-or-error name "read")))


;; Pass 2: convert assignments (step 1 of closure conversion)
;; if a variable is 1) free in a lambda and 2) written to by a set!, then it
;; needs to be boxed, and references to it translated into dereferences.
(defparameter *scheme-2* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-2"))

(defparameter *s2-captured-vars* (fset:empty-set) "Tracks captured variables")
(defparameter *s2-assigned-vars* (fset:empty-set) "Tracks assigned-to variables")

(def-op *scheme-2* (:module &rest body)
  (let ((*s2-captured-vars* (fset:empty-set))
        (*s2-assigned-vars* (fset:empty-set)))
    (append (list :module) (mapcan #'recurse-splice body))))

(def-op *scheme-2* (:set! var form)
  ;; if variable is free in lambda, add to list of variables to box
  (when (not (fset:contains? *s2-captured-vars* var))
    (setf *s2-assigned-vars* (fset:with *s2-assigned-vars* var)))
  (if (fset:contains? *s2-assigned-vars* var)
    ;; If boxed, access through reference
    (list :set-ref! var form)
    ;; otherwise, keep the same
    (list :set! var form)))

(def-op *scheme-2* (:var var)
  (if (fset:contains? *s2-assigned-vars* var)
    ;; If boxed, access through reference
    (list :deref var)
    ;; otherwise, keep the same
    (list :var var)))

(def-op *scheme-2* (:lambda params &rest body)
  ;; keep track of currently-captured variables
  (let ((*s2-captured-vars* (fset:convert 'fset:set params)))
    (append (list :lambda params) (mapcan #'recurse-splice body))))

(def-op *scheme-2* (:let bindings &rest body)
  ;; recurse to find variables that should be boxed
  (let* ((body* (mapcan #'recurse-splice body))
         (bindings* (mapcar
                      (lambda (binding)
                        ;; If variable should be boxed, convert into make-ref
                        (destructuring-bind (name value) binding
                          (if (fset:contains? *s2-assigned-vars* name)
                            (list name (list :make-ref (recurse value)))
                            (list name (recurse value)))))
                      bindings))
         ;; Recurse again so :var forms are properly substituted
         (body** (mapcan #'recurse-splice body*)))
    (list :let bindings* body**)))

; (let ((*s2-assigned-vars* (fset:empty-set)))
;   (lower *scheme-2*
;          '(:module
;             (:define f (:lambda (x)
;                          (:let ((y 10))
;                            (:lambda (z)
;                              (:set! y 10)
;                              (:var y))))))))


;; Pass 3: convert closures
;; Convert each lambda into a :make-closure element, which captures
;; all free variables into a flat closure. Also lifts lambda body
;; into top-level definition.
(defparameter *scheme-3* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-3"))

(defparameter *s3-lambdas* (fset:empty-map) "Tracks lambdas to convert")



(defparameter *scheme-to-blub* (make-interpreter :on-unknown :recurse
                                                 :readable-name "SCHEME->BLUB"))

;; Define blub functions used by our scheme
(defparameter *scheme-prelude*
  (list '(:function (:type :i32) fib
                    ((:type :i32) n)
                    (:block
                      (:declare (:type :i32) a 0)
                      (:declare (:type :i32) b 1)
                      (:declare (:type :i32) i 0)
                      (:declare (:type :i32) tmp 0)
                      (:while (:lt (:var i) (:var n))
                              (:block
                                (:set tmp (:var b))
                                (:set b   (:add (:var a) (:var b)))
                                (:set a   (:var tmp))
                                (:set i   (:add (:var i) 1))))
                      (:return (:var a))))))

;; Just add in prelude
(def-op *scheme-to-blub* (:module &rest body)
  `(:module ,@*scheme-prelude*
            ,@(mapcan #'recurse-splice body)))
