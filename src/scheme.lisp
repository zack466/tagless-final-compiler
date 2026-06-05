(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

(defun mapappend (fn &rest lists)
  (reduce #'append (apply #'mapcar fn lists)))

(defmacro lower-passes (ast &rest passes)
  (reduce #'(lambda (acc pass) `(lower ,pass ,acc))
          passes :initial-value ast))

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
       :if :let :set! :lambda :block :apply)))

    ;; ugly, but makes it easer to write compiler passes
    (:var (identifier))
    (:apply :form (repeat0 :form))
    (:block (repeat0 :form))

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
;; 5 - dispatch primitives as function calls

;; Pass 0: desugaring
(defparameter *scheme-0* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-0"))

(defparameter *scheme-globals* (fset:empty-set)
  "Set of global variables defined at the module level.")

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
    ;; Pre-register all globals to support mutual recursion and out-of-order references
    (dolist (form body)
      (when (and (consp form) (eq (car form) :define))
        (s1-register-global (second form))))
    (append (list :module) (mapcan #'recurse-splice body))))

(def-op *scheme-1* (:define name form)
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

;; 
;; Part A: just identify AF, set of variables that need ot be converted

(defparameter *scheme-2a* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-2A"))

(defparameter *s2a-captured-vars* (fset:empty-set) "Variables captured by an enclosing lambda")
(defparameter *s2a-vars-to-box*   (fset:empty-set) "Accumulator: variables needing boxing")

(def-op *scheme-2a* (:lambda params &rest body)
  (let ((*s2a-captured-vars* (fset:convert 'fset:set params)))
    (append (list :lambda params) (mapappend #'recurse-splice body))))

(def-op *scheme-2a* (:set! var form)
  (when (and (not (fset:contains? *s2a-captured-vars* var))
             (not (fset:contains? *scheme-globals* var)))
    (setf *s2a-vars-to-box* (fset:with *s2a-vars-to-box* var)))
  (list :set! var (recurse form)))

(defun s2a-vars-to-box (ast)
  (let ((*s2a-captured-vars* (fset:empty-set))
        (*s2a-vars-to-box*   (fset:empty-set)))
    (lower *scheme-2a* ast)
    *s2a-vars-to-box*))

;; Part B: box variables found in previous pass
;;
;; if var v boxed, then:
;;   (:var v)       -> (:deref v)
;;   (:set! v form) -> (:set-ref! v form)
;;   (:let ((v e))) -> wraps e in (:make-ref e)
;;   (:lambda ..)   -> boxed param renamed to a temp; original name
;;                     rebound to (:make-ref (:var temp)) in the body

(defparameter *scheme-2b* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-2B"))

(defparameter *s2b-boxed* (fset:empty-set) "Variables to box (from pass 2a)")

(defun s2b-boxed-p (var) (fset:contains? *s2b-boxed* var))

(def-op *scheme-2b* (:var var)
  (if (s2b-boxed-p var) (list :deref (list :var var)) (list :var var)))

(def-op *scheme-2b* (:set! var form)
  (if (s2b-boxed-p var)
      (list :set-ref! var (recurse form))
      (list :set! var (recurse form))))

(def-op *scheme-2b* (:let bindings &rest body)
  (let ((bindings* (mapcar (lambda (binding)
                             (destructuring-bind (name value) binding
                               (if (s2b-boxed-p name)
                                   (list name (list :make-ref (recurse value)))
                                   (list name (recurse value)))))
                           bindings)))
    (append (list :let bindings*) (mapappend #'recurse-splice body))))

(def-op *scheme-2b* (:lambda params &rest body)
  (let ((rename (mapcar (lambda (p)
                          (if (s2b-boxed-p p) (cons p (fresh-name (string p))) (cons p p)))
                        params)))
    (if (every (lambda (r) (eq (car r) (cdr r))) rename)
        (append (list :lambda params) (mapappend #'recurse-splice body))
        (let ((params*   (mapcar #'cdr rename))
              (let-binds (loop for (orig . temp) in rename
                               when (s2b-boxed-p orig)
                                 collect (list orig (list :make-ref (list :var temp)))))
              (body*     (mapappend #'recurse-splice body)))
          (append (list :lambda params*)
                  (list (append (list :let let-binds) body*)))))))

(def-op *scheme-2b* (:module &rest body)
  (let ((*s2b-boxed* (s2a-vars-to-box (this))))
    (append (list :module) (mapappend #'recurse-splice body))))

(defparameter *code* '(:module
                        (:define f (:lambda (x)
                                            (:let ((y 10))
                                                  (:lambda (z)
                                                           (:set! y 10)
                                                           (:set! x (:add 7 (:if (:lt (:var y) 4) 1 2)))
                                                           (:var y)))))))

; (lower *scheme-2b* *code*)


;; Pass 3: convert closures
;; Convert each lambda into a :make-closure element, which captures
;; all free variables into a flat closure. Also lifts lambda body
;; into top-level definition.

;; Pass 3a: annotate lambdas with their free variables
;;
;; Rewrites each lambda from
;;   (:lambda (params) ...body)
;; into
;;   (:lambda (params) (free-vars) ...body)

(defparameter *scheme-3a* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-3A"))

(defparameter *s3a-free* (fset:empty-set)
  "Accumulator: free variables of the body currently being walked.")

(defun s3a-walk-body (body rs)
  "Recurse into BODY under a fresh accumulator; return (values rewritten-body
   free-set), where free-set is what BODY referenced. Does not touch the
   caller's *s3a-free*."
  (let ((*s3a-free* (fset:empty-set)))
    (let ((body* (mapappend rs body)))
      (values body* *s3a-free*))))

(def-op *scheme-3a* (:var name)
  (setf *s3a-free* (fset:with *s3a-free* name))
  (list :var name))

(def-op *scheme-3a* (:set! var form)
  ;; the form is walked (its vars accumulate); the assigned name itself
  ;; is a reference to a binding too, so count it free
  (let ((form* (recurse form)))
    (setf *s3a-free* (fset:with *s3a-free* var))
    (list :set! var form*)))

(def-op *scheme-3a* (:let bindings &rest body)
  (let* ((names     (fset:convert 'fset:set (mapcar #'first bindings)))
         ;; binding values evaluated in enclosing scope -> normal recurse,
         ;; their vars land in the current accumulator
         (bindings* (mapcar (lambda (b) (list (first b) (recurse (second b))))
                            bindings)))
    (multiple-value-bind (body* body-free) (s3a-walk-body body #'recurse-splice)
      ;; vars free in the body but not bound here escape to enclosing scope
      (setf *s3a-free*
            (fset:union *s3a-free* (fset:set-difference body-free names)))
      (append (list :let bindings*) body*))))

(def-op *scheme-3a* (:lambda params &rest body)
  (let ((pset (fset:convert 'fset:set params)))
    (multiple-value-bind (body* body-free) (s3a-walk-body body #'recurse-splice)
      (let* ((free (fset:set-difference body-free pset))
             (free (fset:set-difference free *scheme-globals*)))
        ;; this lambda's free vars are free in the enclosing scope too
        (setf *s3a-free* (fset:union *s3a-free* free))
        (append (list :lambda params (fset:convert 'list free)) body*)))))

(def-op *scheme-3a* (:module &rest body)
  (let ((*s3a-free* (fset:empty-set)))
    (append (list :module) (mapappend #'recurse-splice body))))


;; Pass 3b: convert closures with free variables
;;
;; Perform as follows:
;; take (lambda <params> <free-vars> ...<body>)
;; and produce a top-level function:
;;
;; (function <name> (...<params>)
;;   (let (...(free-vars <free-vars>))
;;     ...<body>))
;;
;; and return:
;; (make-closure <name> ...<free-vars>)

(defparameter *scheme-3b* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-3B"))

(defparameter *s3a-lambdas* (fset:empty-map)
  "Mappings from lambda symbols to their top-level representations")

(defun s3a-construct-lambda (name params free-vars body)
  (let* ((closure-var (fresh-name "closure"))
        (free-vars-map
          (loop for sym in free-vars
            for idx from 0
            collect (list sym (list :free-var idx (list :var closure-var))))))
    (if (not (null free-vars-map))
      (list :function name (cons closure-var params)
            (append (list :let free-vars-map) body))
      (append (list :function name (cons closure-var params)) body))))

; (s3a-construct-lambda 'asdf (list 'a 'b 'c) (list 'x 'y 'z) (list (list '+ 'a 'b 'c 'x 'y 'z)))

(def-op *scheme-3b* (:module &rest body)
  (let* ((*s3a-lambdas* (fset:empty-map))
         (body* (mapappend #'recurse-splice body)))
    (append (list :module)
            (fset:convert 'list (fset:range *s3a-lambdas*))
            body*)))

(def-op *scheme-3b* (:lambda params free-vars &rest body)
  ;; First recurse on the body
  (let* ((body* (mapappend #'recurse-splice body))
         (fn-name (fresh-name "lambda"))
         (fn (s3a-construct-lambda fn-name params free-vars body*)))
    ;; construct top-level function AST
    (setf *s3a-lambdas* (fset:with *s3a-lambdas* fn-name fn))
    ;; return make-closure call. The fn-ptr is cast to :u64 so the runtime
    ;; helper can accept a uniform u64 first argument.
    (append (list :make-closure
                  (list :cast '(:type :u64) (list :fn-ptr fn-name))
                  (length free-vars))
            (loop for fv in free-vars collect (list :var fv)))))


(lower *scheme-3b* (lower *scheme-3a* (lower *scheme-2b* (lower *scheme-2a* *code*))))

;; Pass 4
;; Remove complex operands:
;; - if, set!, block
;;
;; if any of these forms exist where a primitive is needed, need to bind to a
;; temporary first using let.
;;
;; (+ (block (set! x 10 ) x) (block y))
;; ->
;; (let ((tmp1 ((block (set! x 10) x))))
;;   (let ((tmp2 (block y)))
;;     (+ tmp1 tmp2)))
;;

;; Primitive operations may only take ATOMIC operands -- a :var or a
;; literal. A complex operand (:if, :let, :set!, :apply, a nested
;; primitive, ...) is lifted: lower it, bind it to a fresh temp, and use
;; the temp in operand position. The wrapping lets stack around the
;; primitive.
;;
;; *s4-bindings* accumulates (temp value) pairs while a primitive's
;; operands are being atomized. s4-atomize handles one operand: atomic
;; ones pass through; complex ones get a fresh temp and push a binding.

(defparameter *scheme-4* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-4"))

(defparameter *s4-bindings* nil
  "Accumulator (reverse order) of (temp value) pairs for the primitive
   currently being atomized.")

(defun s4-atomic-p (expr)
  "An operand is atomic if it needs no temp: a variable or a literal."
  (or (not (consp expr))
      (eq (first expr) :var)))

(defun s4-atomize (operand)
  "Return an atomic stand-in for OPERAND, pushing a (temp value) binding
   onto *s4-bindings* if OPERAND was complex."
  (let ((operand* (lower *scheme-4* operand)))
    (if (s4-atomic-p operand*)
        operand*
        (let ((tmp (fresh-name "tmp")))
          (push (list tmp operand*) *s4-bindings*)
          (list :var tmp)))))

(defun s4-wrap (bindings body)
  "Wrap BODY in nested lets, one per binding (outermost = first bound)."
  (reduce (lambda (binding acc) (list :let (list binding) acc))
          bindings
          :initial-value body
          :from-end t))

;; Apply -- treated as a primitive: function and all args must be atomic,
;; since this lowers to a C-level call apply(fn, ...args).
(def-op *scheme-4* (:apply fn &rest args)
  (let* ((*s4-bindings* nil)
         (fn*   (s4-atomize fn))
         (args* (mapcar #'s4-atomize args)))
    (s4-wrap (nreverse *s4-bindings*)
             (list* :apply fn* args*))))

(defmacro def-s4-prim (keyword &rest argnames)
  "Define a pass-4 handler for a primitive KEYWORD with fixed arity:
   atomize each operand, then wrap the rebuilt primitive in the lets
   collected for those operands."
  `(def-op *scheme-4* (,keyword ,@argnames)
     (let* ((*s4-bindings* nil)
            (args* (list ,@(mapcar (lambda (a) `(s4-atomize ,a)) argnames))))
       (s4-wrap (nreverse *s4-bindings*)
                (cons ,keyword args*)))))

;; Unary
(def-s4-prim :neg a)
(def-s4-prim :not a)
(def-s4-prim :car a)
(def-s4-prim :cdr a)
;; Binary arithmetic / bitwise
(def-s4-prim :add a b)
(def-s4-prim :sub a b)
(def-s4-prim :mul a b)
(def-s4-prim :div a b)
(def-s4-prim :and a b)
(def-s4-prim :or  a b)
(def-s4-prim :xor a b)
;; Comparison
(def-s4-prim :eq a b)
(def-s4-prim :ne a b)
(def-s4-prim :lt a b)
(def-s4-prim :le a b)
(def-s4-prim :gt a b)
(def-s4-prim :ge a b)
;; Pairs
(def-s4-prim :cons a b)


(lower-passes
  *code*
  *scheme-2a*
  *scheme-2b*
  *scheme-3a*
  *scheme-3b*
  *scheme-4*)


;; Pass 5
;;
;; Explicate control (normalize let and if), make execution order part of
;; syntax
;;
;; (let ((x (let ((z 1)) z)) (y 2))
;;   ...body)
;;  turns into
;; (let ((z 1))
;;   (let ((x z) (y 2))
;;   ...body))
;;  )

;; ============================================================
;; Pass 5: explicate control (normalize :let and :if)
;; ============================================================
;; Make execution order syntactic: a :let binding value may no longer be
;; a compound :let or :if. Such a value is hoisted into a binding that
;; sequences in front.
;;
;;   (:let ((x (:let ((z 1)) z)) (y 2)) body)
;;   => (:let ((z 1)) (:let ((x z) (y 2)) body))
;;
;;   (:let ((x (:if c a b))) body)
;;   => (:let ((x (:if c a b))) body)   ; x's value is a temp, see below
;;
;; *s5-hoisted* accumulates (name value) bindings lifted out of the
;; binding list currently being normalized.

(defparameter *scheme-5* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-5"))

(defparameter *s5-hoisted* nil
  "Accumulator (reverse order) of (name value) bindings hoisted out of the
   let-binding list currently being processed.")

(defun s5-simple-value-p (expr)
  "A binding value is simple if it needn't be hoisted: anything that is
   not a compound :let or :if form."
  (not (and (consp expr)
            (member (first expr) '(:let :if)))))

(defun s5-normalize-value (value)
  "Lower VALUE; if it is still a compound :let/:if, bind it to a fresh
   temp pushed onto *s5-hoisted* and return the temp reference. Otherwise
   return the lowered value."
  (let ((value* (lower *scheme-5* value)))
    (if (s5-simple-value-p value*)
        value*
        (let ((tmp (fresh-name "t")))
          (push (list tmp value*) *s5-hoisted*)
          (list :var tmp)))))

(defun s5-wrap (bindings body)
  "Wrap BODY in nested single-binding lets, outermost = first bound."
  (reduce (lambda (binding acc) (list :let (list binding) acc))
          bindings
          :initial-value body
          :from-end t))

(def-op *scheme-5* (:let bindings &rest body)
  (let* ((*s5-hoisted* nil)
         ;; normalize each binding value; complex ones hoist out in front
         (bindings* (mapcar (lambda (b)
                              (list (first b) (s5-normalize-value (second b))))
                            bindings))
         (body*     (mapappend #'recurse-splice body)))
    ;; the hoisted bindings sequence in front of this (now-flat) let
    (s5-wrap (nreverse *s5-hoisted*)
             (append (list :let bindings*) body*))))

;; --- :set! / :set-ref! : normalize the assigned value ---
;; The value being assigned is a value position just like a let binding,
;; so a compound :let/:if there gets hoisted to a temp in front.
;;
;;   (:set-ref! x (:if c a b))
;;   => (:let ((t (:if c a b))) (:set-ref! x (:var t)))

(def-op *scheme-5* (:set-ref! var form)
  (let* ((*s5-hoisted* nil)
         (form* (s5-normalize-value form)))
    (s5-wrap (nreverse *s5-hoisted*)
             (list :set-ref! var form*))))

(def-op *scheme-5* (:set! var form)
  (let* ((*s5-hoisted* nil)
         (form* (s5-normalize-value form)))
    (s5-wrap (nreverse *s5-hoisted*)
             (list :set! var form*))))

;; In pass 5 -- normalize the :if condition, hoisting compound conditions
;; out in front (same treatment as :set! values and :let bindings).
(def-op *scheme-5* (:if cond then &optional else)
  (let* ((*s5-hoisted* nil)
         (cond* (s5-normalize-value cond)))
    (s5-wrap (nreverse *s5-hoisted*)
             (append (list :if cond* (recurse then))
                     (when else (list (recurse else)))))))

(lower-passes
  *code*
  *scheme-2a*
  *scheme-2b*
  *scheme-3a*
  *scheme-3b*
  *scheme-4*
  *scheme-5*
  )

;; ============================================================
;; Pass 6: flatten nested :let/:if into blub statements
;; ============================================================
;; blub splits into statements and expressions. A scheme :if is an
;; expression (has a value), but blub :if is a statement (no value), so a
;; value-producing :if must predeclare a destination and assign into it
;; from each branch. Every form in value position carries a DISPOSITION
;; saying what to do with its result:
;;   (:set . VAR) -- assign the value to VAR
;;   :return      -- return the value
;;   :effect      -- discard the value (expression statement)
;;
;; Two cooperating interpreters mirror blub's own statement/expr split:
;;   *scheme-6*  -- statement world: :let/:if/:set-ref! flatten and emit
;;                  into *s6-stmts*; value forms are disposed.
;;   *scheme-6e* -- expression world: pure node->node translation of a
;;                  value form into a blub expr (:apply -> :call, etc).
;;
;; Dynamic state:
;;   *s6-stmts* -- reverse-order statement accumulator for current block
;;   *s6-disp*  -- disposition for the form currently being flattened
;;
;; Pass 5 has hoisted every compound :let/:if out of arbitrary value slots
;; (let bindings AND set values), so in statement position an :if/:let only
;; appears as a let binding value, in tail position, or in effect position.

(defparameter *scheme-6* (make-interpreter :on-unknown :recurse
                                           :readable-name "SCHEME-6"))
(defparameter *scheme-6e* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-6E"))

(defparameter +u64+ '(:type :u64))
(defparameter *s6-stmts* nil    "Reverse-order statement accumulator.")
(defparameter *s6-disp*  :effect "How to dispose of the current value.")

(defun s6-emit (stmt) (push stmt *s6-stmts*))

(defun s6-dispose (expr)
  "Emit the statement that disposes of blub-expr EXPR per *s6-disp*."
  (s6-emit (cond ((consp *s6-disp*)      (list :set (cdr *s6-disp*) expr))
                 ((eq *s6-disp* :return) (list :return expr))
                 (t                      (list :expr expr)))))

(defun s6-statement-form-p (form)
  "True if FORM is flattened by a statement handler (vs. a value expr)."
  (and (consp form) (member (first form) '(:let :if :set-ref!))))

;; --- expression world: value form -> blub expr ---

(def-op *scheme-6e* (:apply fn &rest args)
  ;; Inline the closure call: every scheme value is :u64 and every compiled
  ;; lambda has signature (env: u64, arg1: u64, ..., argN: u64) -> u64.
  ;; Extract the function pointer and closure base from FN, cast to the right
  ;; fn-type, and dispatch directly. FN is atomic post pass 4 (a :var), so
  ;; it is safe to evaluate twice.
  (let* ((fn-val (recurse fn))
         (args*  (mapcar #'recurse args))
         (n+1    (1+ (length args)))
         (params (loop repeat n+1 collect '(:type :u64)))
         (fn-type `(:type (:fn (:type :u64) ,@params))))
    `(:call (:cast ,fn-type (:call _closure_fn ,fn-val))
            (:call _closure_env ,fn-val)
            ,@args*)))

;; :var, :deref, :add, :<, :make-closure, :make-ref, literals, etc. ride
;; the :recurse policy -- they rebuild with operands lowered, which is
;; already their blub shape.

;; --- statement world ---

;; A single form in statement position: statement-producers recurse in
;; *scheme-6* (they self-emit and return nil); value forms translate via
;; *scheme-6e* then dispose per *s6-disp*. Wrapped as an operator so that
;; recurse / cross-interpreter lower are in scope inside the handler.
(def-op *scheme-6* (:s6-stmt form)
  (if (s6-statement-form-p form)
      (recurse form)
      (s6-dispose (lower *scheme-6e* form)))
  nil)

;; Flatten FORMS into a fresh (:block ...). Only the last form inherits the
;; ambient disposition; earlier forms run for effect.
(def-op *scheme-6* (:s6-block &rest forms)
  (let ((*s6-stmts* nil))
    (loop for (form . rest) on forms
          for last-p = (null rest)
          do (let ((*s6-disp* (if last-p *s6-disp* :effect)))
               (lower *scheme-6* (list :s6-stmt form))))
    (cons :block (nreverse *s6-stmts*))))

(def-op *scheme-6* (:let bindings &rest body)
  ;; declares emit into the current block; an :if/:let-valued binding
  ;; declares uninitialized and assigns into the name from its branches
  (dolist (b bindings)
    (destructuring-bind (name value) b
      (if (s6-statement-form-p (cons (if (consp value) (first value) nil) nil))
          nil nil)                      ; (placeholder, replaced just below)
      (if (and (consp value) (member (first value) '(:let :if)))
          (let ((*s6-disp* (cons :set name)))
            (s6-emit (list :declare +u64+ name))
            (recurse value))
          (s6-emit (list :declare +u64+ name (lower *scheme-6e* value))))))
  ;; body: only the last form inherits this :let's disposition
  (loop for (form . rest) on body
        for last-p = (null rest)
        do (let ((*s6-disp* (if last-p *s6-disp* :effect)))
             (lower *scheme-6* (list :s6-stmt form))))
  nil)

(def-op *scheme-6* (:if cond then &optional else)
  ;; condition is a pure expression; each branch inherits the current
  ;; disposition (so its tail value is assigned/returned/discarded alike)
  (let ((c (lower *scheme-6e* cond)))
    (s6-emit
     (append (list :if c (lower *scheme-6* (list :s6-block then)))
             (when else
               (list (lower *scheme-6* (list :s6-block else)))))))
  nil)

(def-op *scheme-6* (:set-ref! var form)
  ;; a set is pure effect regardless of surrounding disposition
  (s6-emit (list :set var (lower *scheme-6e* form)))
  nil)

;; --- top level ---

(def-op *scheme-6* (:function name params &rest body)
  (let ((*s6-disp* :return))
    (append (list :function name params)        ; <- params kept as one list
            (list (lower *scheme-6* (cons :s6-block body))))))

(def-op *scheme-6* (:define name form)
  ;; a top-level value definition -> blub :global
  (list :global +u64+ name (lower *scheme-6e* form)))

(def-op *scheme-6* (:module &rest body)
  (cons :module (mapappend #'recurse-splice body)))

;; ============================================================
;; Pass 6a: type annotation
;; ============================================================
;; blub requires types on function return + params, declares, and globals.
;; Every scheme value is a u64 (tagged), so every type is :u64.
;;   (:function name (p ..) block)        -> (:function (:type :u64) name ((:type :u64) p) .. block)
;;   (:declare name expr) | (:declare name) -> (:declare (:type :u64) name [expr])
;;   (:global name expr)                   -> (:global (:type :u64) name [expr])
;;
;; NOTE: pass 6 emits :declare already carrying +u64+ and (:function name
;; params block) WITHOUT a return type or typed params. 6a normalizes the
;; function header; declares from pass 6 are already typed, but 6a is
;; idempotent on an already-typed declare so it's safe either way.

(defparameter *scheme-6a* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-6A"))

(defparameter +u64+ '(:type :u64))

(defun s6a-typed-p (x)
  "True if X is already a (:type ...) form."
  (and (consp x) (eq (first x) :type)))

(def-op *scheme-6a* (:function name params &rest rest)
  ;; rest is (block) -- a single (:block ...). Recurse into it so any
  ;; nested declares/globals get normalized too.
  (let ((params* (mapcar (lambda (p)
                           ;; p is either NAME (untyped) or ((:type T) NAME)
                           (if (and (consp p) (s6a-typed-p (first p)))
                               p
                               (list +u64+ p)))
                         params)))
    (append (list :function +u64+ name)
            params*
            (mapcar #'recurse rest))))

(def-op *scheme-6a* (:declare &rest args)
  ;; args is either (NAME) | (NAME EXPR) | ((:type T) NAME [EXPR])
  (if (s6a-typed-p (first args))
      (cons :declare (cons (first args)
                           (cons (second args)
                                 (mapcar #'recurse (cddr args)))))
      (destructuring-bind (name &optional (expr nil expr-p)) args
        (if expr-p
            (list :declare +u64+ name (recurse expr))
            (list :declare +u64+ name)))))

(def-op *scheme-6a* (:global &rest args)
  (if (s6a-typed-p (first args))
      (cons :global (cons (first args)
                          (cons (second args)
                                (mapcar #'recurse (cddr args)))))
      (destructuring-bind (name &optional (expr nil expr-p)) args
        (if expr-p
            (list :global +u64+ name (recurse expr))
            (list :global +u64+ name)))))

;; everything else (:module, :block, :if, :while, :set, :return, exprs)
;; rides :recurse so nested functions/declares are reached.

;; ============================================================
;; Pass 6b: lower scheme primitives to builtin calls
;; ============================================================
;; Scheme-level primitive operators become calls to runtime builtins that
;; do the tag-checking / tag-stripping:
;;   (:add a b) -> (:call _add a b)   ... and sub/mul/div/and/or/xor
;;   (:eq a b)  -> (:call _eq  a b)   ... and ne/lt/le/gt/ge
;;   (:neg a)   -> (:call _neg a)     ... and not
;;   (:cons a b)-> (:call _cons a b)
;;   (:car a)   -> (:call _car a) ; (:cdr a) -> (:call _cdr a)
;;
;; The builtin name is the operator keyword spelled as a symbol with a
;; leading underscore (_add, _eq, ...). Operands are recursed first so
;; nested primitives lower too.

(defparameter *scheme-6b* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-6B"))

(defparameter +s6b-prims+
  '(:neg :not :add :sub :mul :div :and :or :xor
    :eq :ne :lt :le :gt :ge :cons :car :cdr)
  "Scheme primitive operators that lower to builtin calls.")

(defun s6b-builtin-name (keyword)
  "Map a primitive KEYWORD to its builtin function symbol:
   :add -> _ADD, :make-closure -> _MAKE_CLOSURE.
   Preserves uppercase and converts hyphens to underscores so the generated
   symbol matches the names used in *scheme-runtime*."
  (intern (format nil "_~A"
                  (substitute #\_ #\- (symbol-name keyword)))))

(defmacro def-s6b-prim (keyword)
  "Define a 6b handler turning (KEYWORD . operands) into
   (:call <builtin> . lowered-operands)."
  `(def-op *scheme-6b* (,keyword &rest operands)
     (list* :call (s6b-builtin-name ,keyword)
            (mapcar #'recurse operands))))

;; generate a handler per primitive
(macrolet ((gen () `(progn ,@(mapcar (lambda (k) `(def-s6b-prim ,k))
                                     '(:neg :not :add :sub :mul :div
                                       :and :or :xor :eq :ne :lt :le
                                       :gt :ge :cons :car :cdr
                                       :make-ref :deref)))))
  (gen))

(def-op *scheme-6b* (:free-var idx closure)
  (list :call (s6b-builtin-name :free-var)
        (list :cast '(:type :u64) (recurse idx))
        (recurse closure)))

;; :make-closure is special: the runtime helper is variadic in the free-var
;; values, so the call site must include a (:varargs) marker between the
;; (fn, count) prefix and the free-var args.
(def-op *scheme-6b* (:make-closure fn n &rest free-vars)
  (list* :call (s6b-builtin-name :make-closure)
         (recurse fn)
         (list :cast '(:type :u64) (recurse n))
         '(:varargs)
         (mapcar #'recurse free-vars)))


(pp-blub (lower-passes
   *code*
   *scheme-2a*
   *scheme-2b*
   *scheme-3a*
   *scheme-3b*
   *scheme-4*
   *scheme-5*
   *scheme-6*
   *scheme-6a*
   *scheme-6b*
   ))

(defparameter *scheme-to-blub* (make-interpreter :on-unknown :recurse
                                                 :readable-name "SCHEME->BLUB"))

;; ============================================================
;; Scheme runtime (written in blub)
;; ============================================================
;; Every scheme value is a u64. The low 4 bits encode a type tag and the
;; remaining bits hold either a shifted integer or an aligned heap pointer
;; (malloc returns 16-byte-aligned, so the low 4 bits start zero).
;;
;;   TAG_INT  = 0   value     = (n << 4) | 0
;;   TAG_PAIR = 1   value     = ptr | 1   ; ptr -> [u64 car, u64 cdr]
;;   TAG_CLOS = 2   value     = ptr | 2   ; ptr -> [u64 fn, u64 free0, ...]
;;   TAG_REF  = 3   value     = ptr | 3   ; ptr -> [u64 cell]
;;
;; Integer literals are pre-shifted at compile time by SCHEME-BOX-LITERALS,
;; so the runtime never has to box them. Each primitive checks its operand
;; tags before doing work; on mismatch it calls _scheme_panic which aborts.

(defparameter *scheme-runtime*
  '(
    ;; C-runtime entry points.
    (:extern (:type :u64)  _scheme_alloc ((:type :u64) bytes))
    (:extern (:type :u64)  _scheme_panic)

    ;; ---- type-tag checks ----
    ;; Each binop primitive begins (:if (:or (tagbits a) (tagbits b)) panic).
    ;; tagbits = (:and v 15); non-zero means tag != TAG_INT.

    ;; ---- arithmetic / bitwise (ints only) ----

    (:function (:type :u64) _add ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:add (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _sub ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:sub (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _mul ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:mul (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _div ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:div (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _and ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:and (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _or ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:or (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _xor ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:xor (:shr (:var a) 4) (:shr (:var b) 4)) 4))))

    (:function (:type :u64) _neg ((:type :u64) a)
      (:block
        (:if (:and (:var a) 15)
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:neg (:shr (:var a) 4)) 4))))

    (:function (:type :u64) _not ((:type :u64) a)
      (:block
        (:if (:and (:var a) 15)
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:not (:shr (:var a) 4)) 4))))

    ;; ---- comparisons (return boxed 0/1) ----

    (:function (:type :u64) _eq ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:cast (:type :u64) (:eq (:var a) (:var b))) 4))))

    (:function (:type :u64) _ne ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:cast (:type :u64) (:ne (:var a) (:var b))) 4))))

    (:function (:type :u64) _lt ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:cast (:type :u64) (:lt (:shr (:var a) 4) (:shr (:var b) 4))) 4))))

    (:function (:type :u64) _le ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:cast (:type :u64) (:le (:shr (:var a) 4) (:shr (:var b) 4))) 4))))

    (:function (:type :u64) _gt ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:cast (:type :u64) (:gt (:shr (:var a) 4) (:shr (:var b) 4))) 4))))

    (:function (:type :u64) _ge ((:type :u64) a) ((:type :u64) b)
      (:block
        (:if (:or (:and (:var a) 15) (:and (:var b) 15))
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:return (:shl (:cast (:type :u64) (:ge (:shr (:var a) 4) (:shr (:var b) 4))) 4))))

    ;; ---- pairs ----

    (:function (:type :u64) _cons ((:type :u64) a) ((:type :u64) b)
      (:block
        (:declare (:type :u64) raw (:call _scheme_alloc 16))
        (:declare (:type (:pointer (:type :u64))) p0
                  (:cast (:type (:pointer (:type :u64))) (:var raw)))
        (:set (:deref (:var p0)) (:var a))
        (:declare (:type (:pointer (:type :u64))) p1
                  (:cast (:type (:pointer (:type :u64))) (:add (:var raw) 8)))
        (:set (:deref (:var p1)) (:var b))
        (:return (:or (:var raw) 1))))

    (:function (:type :u64) _car ((:type :u64) v)
      (:block
        (:if (:ne (:and (:var v) 15) 1)
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:declare (:type (:pointer (:type :u64))) p
                  (:cast (:type (:pointer (:type :u64))) (:sub (:var v) 1)))
        (:return (:deref (:var p)))))

    (:function (:type :u64) _cdr ((:type :u64) v)
      (:block
        (:if (:ne (:and (:var v) 15) 1)
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:declare (:type (:pointer (:type :u64))) p
                  (:cast (:type (:pointer (:type :u64))) (:add (:sub (:var v) 1) 8)))
        (:return (:deref (:var p)))))

    ;; ---- refs (single-cell boxes) ----

    (:function (:type :u64) _make_ref ((:type :u64) v)
      (:block
        (:declare (:type :u64) raw (:call _scheme_alloc 8))
        (:declare (:type (:pointer (:type :u64))) p
                  (:cast (:type (:pointer (:type :u64))) (:var raw)))
        (:set (:deref (:var p)) (:var v))
        (:return (:or (:var raw) 3))))

    (:function (:type :u64) _deref ((:type :u64) v)
      (:block
        (:if (:ne (:and (:var v) 15) 3)
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:declare (:type (:pointer (:type :u64))) p
                  (:cast (:type (:pointer (:type :u64))) (:sub (:var v) 3)))
        (:return (:deref (:var p)))))

    ;; ---- closures ----
    ;; layout: [u64 fn, u64 free0, u64 free1, ...].
    ;; _make_closure(fn, n, ...vals) — variadic in n free vars.

    (:function (:type :u64) _make_closure ((:type :u64) fn) ((:type :u64) n) (:varargs)
      (:block
        (:declare (:type :valist) ap)
        (:vastart ap)
        (:declare (:type :u64) sz  (:add 8 (:shl (:var n) 3)))
        (:declare (:type :u64) raw (:call _scheme_alloc (:var sz)))
        (:declare (:type (:pointer (:type :u64))) p0
                  (:cast (:type (:pointer (:type :u64))) (:var raw)))
        (:set (:deref (:var p0)) (:var fn))
        (:declare (:type :u64) i 0)
        (:while (:lt (:var i) (:var n))
          (:block
            (:declare (:type :u64) v (:vaarg ap (:type :u64)))
            (:declare (:type :u64) offset (:add 8 (:shl (:var i) 3)))
            (:declare (:type (:pointer (:type :u64))) slot
                      (:cast (:type (:pointer (:type :u64))) (:add (:var raw) (:var offset))))
            (:set (:deref (:var slot)) (:var v))
            (:set i (:add (:var i) 1))))
        (:return (:or (:var raw) 2))))

    (:function (:type :u64) _free_var ((:type :u64) i) ((:type :u64) closure)
      (:block
        (:declare (:type :u64) offset (:add 8 (:shl (:var i) 3)))
        (:declare (:type (:pointer (:type :u64))) slot
                  (:cast (:type (:pointer (:type :u64)))
                         (:add (:var closure) (:var offset))))
        (:return (:deref (:var slot)))))

    ;; _closure_fn(v) -> raw function pointer at closure[0].
    (:function (:type :u64) _closure_fn ((:type :u64) v)
      (:block
        (:if (:ne (:and (:var v) 15) 2)
          (:block (:declare (:type :u64) _ (:call _scheme_panic))))
        (:declare (:type (:pointer (:type :u64))) p
                  (:cast (:type (:pointer (:type :u64))) (:sub (:var v) 2)))
        (:return (:deref (:var p)))))

    ;; _closure_env(v) -> untagged closure pointer (passed as the env arg).
    (:function (:type :u64) _closure_env ((:type :u64) v)
      (:block
        (:return (:sub (:var v) 2))))))

;; ============================================================
;; Literal boxing
;; ============================================================
;; SCHEME-BOX-LITERALS pre-shifts every integer literal in the source AST so
;; that user code does not have to call a _box_int helper at runtime. Runs
;; before any compiler-introduced integers (closure free-var counts/indices,
;; etc.) appear in the tree, so those stay raw.

(defun scheme-box-literals (ast)
  (cond
    ((integerp ast) (ash ast 4))
    ((consp ast)    (mapcar #'scheme-box-literals ast))
    (t              ast)))

;; ============================================================
;; scheme -> blub driver
;; ============================================================
;; Splits each global initializer into a zero-initialized data slot plus a
;; statement in _scheme_init_globals.  Adds a qbe_main driver that calls
;; _scheme_init_globals and then applies a closure named scheme `main` (if
;; present).  The scheme symbol `main` is renamed to `_scheme_main` to avoid
;; colliding with the C-driver's int main().

(defun split-scheme-global (g)
  "Return (zero-init-global . init-stmt-or-nil)."
  (destructuring-bind (kw type name &optional init) g
    (cond
      ((or (null init) (numberp init))
       (cons g nil))
      (t
       (cons (list kw type name 0)
             (list :set name init))))))

(defun rename-scheme-main (form)
  "Walk FORM replacing the scheme symbol main with _scheme_main everywhere."
  (cond
    ((eq form 'main) '_scheme_main)
    ((consp form)    (mapcar #'rename-scheme-main form))
    (t               form)))

(defun scheme-has-main-p (body)
  (some (lambda (n)
          (and (consp n) (eq (car n) :global) (eq (third n) '_scheme_main)))
        body))

(defun scheme-driver ()
  "Blub form for qbe_main: init globals, apply (main), unbox return."
  '(:function (:type :i32) qbe_main
     (:block
       (:declare (:type :u64) _init (:call _scheme_init_globals))
       (:declare (:type :u64) m (:var _scheme_main))
       (:declare (:type :u64) fnp (:call _closure_fn (:var m)))
       (:declare (:type :u64) env (:call _closure_env (:var m)))
       (:declare (:type :u64) r
                 (:call (:cast (:type (:fn (:type :u64) (:type :u64)))
                               (:var fnp))
                        (:var env)))
       (:return (:cast (:type :i32) (:shr (:var r) 4))))))

(def-op *scheme-to-blub* (:module &rest body)
  (let* ((body*   (mapcan #'recurse-splice body))
         (body*   (rename-scheme-main body*))
         (globals (remove-if-not (lambda (n) (and (consp n) (eq (car n) :global)))
                                 body*))
         (rest    (remove-if (lambda (n) (and (consp n) (eq (car n) :global)))
                             body*))
         (split        (mapcar #'split-scheme-global globals))
         (zero-globals (mapcar #'car split))
         (init-stmts   (remove nil (mapcar #'cdr split)))
         (init-fn      `(:function (:type :u64) _scheme_init_globals
                          (:block ,@init-stmts (:return 0))))
         (driver       (when (scheme-has-main-p zero-globals)
                         (list (scheme-driver)))))
    `(:module
      ,@*scheme-runtime*
      ,@rest
      ,@zero-globals
      ,init-fn
      ,@driver)))

;; ============================================================
;; Pipeline orchestrator
;; ============================================================

(defun sanitize-symbol-name (sym)
  "Sanitize a symbol by replacing any invalid QBE characters (like hyphens) with underscores."
  (if (and sym (symbolp sym))
      (if (eq (symbol-package sym) (find-package :keyword))
          sym
          (let* ((name (symbol-name sym))
                 (first-char (char name 0))
                 (sanitized (map 'string
                                 (lambda (c)
                                   (if (or (alphanumericp c) (char= c #\_))
                                       c
                                       #\_))
                                 name)))
            ;; QBE symbols/temporaries cannot start with a digit.
            ;; If it starts with a digit, prepend an underscore.
            (if (digit-char-p first-char)
                (setf sanitized (format nil "_~A" sanitized)))
            (intern sanitized (symbol-package sym))))
      sym))

(defun sanitize-scheme-ast (ast)
  "Walk AST and sanitize all symbols to contain only valid QBE/C characters."
  (cond
    ((consp ast)   (mapcar #'sanitize-scheme-ast ast))
    ((symbolp ast) (sanitize-symbol-name ast))
    (t             ast)))

(defun compile-scheme (ast &key (name nil) (build-dir "build") (trace nil))
  "Run the full scheme pipeline on AST, returning a blub module."
  (flet ((dump (label form)
           (when (and trace name)
             (let ((path (format nil "~a/~a.scheme-~a.lisp" build-dir name label)))
               (ensure-directories-exist path)
               (with-open-file (s path :direction :output :if-exists :supersede)
                 (write-string (pp-blub form) s))
               (format t ";; [trace] wrote ~a~%" path)))
           form))
    (let* ((ast-sanitized (sanitize-scheme-ast ast))
           (globals (loop for form in (cdr ast-sanitized) ; ast-sanitized is (:module ...)
                          when (and (consp form) (eq (car form) :define))
                          collect (second form)))
           (*scheme-globals* (fset:convert 'fset:set globals)))
      (let* ((p0  (dump "pass0-box" (scheme-box-literals ast-sanitized)))
             (p1  (dump "pass1-uniquify" (lower *scheme-1* p0)))
             (p2b (dump "pass2b-box-rewrite" (lower *scheme-2b* p1)))
             (p3a (dump "pass3a-free-vars" (lower *scheme-3a* p2b)))
             (p3b (dump "pass3b-closure" (lower *scheme-3b* p3a)))
             (p4  (dump "pass4-flatten-let" (lower *scheme-4* p3b)))
             (p5  (dump "pass5-explicate" (lower *scheme-5* p4)))
             (p6  (dump "pass6-flatten-statements" (lower *scheme-6* p5)))
             (p6a (dump "pass6a-types" (lower *scheme-6a* p6)))
             (p6b (dump "pass6b-primitives" (lower *scheme-6b* p6a)))
             (p7  (dump "pass7-to-blub" (lower *scheme-to-blub* p6b))))
        p7))))

(defun build-scheme-ast (ast &key (name "scheme") (build-dir "build") runtime-c
                                  (keep-temp-files t) (trace nil))
  "Compile a scheme AST end-to-end to an executable: scheme -> blub -> qbe."
  (build-blub-ast (compile-scheme ast :name name :build-dir build-dir :trace trace)
                  :name name :build-dir build-dir
                  :runtime-c runtime-c
                  :keep-temp-files keep-temp-files
                  :trace trace))

(defun run-scheme-examples (&key
                               (examples-dir "examples/scheme")
                               (build-dir    "build")
                               (runtime-c    "scheme-runtime.c")
                               (trace        nil))
  "Build and run every *.lisp file under EXAMPLES-DIR.

   Each file may contain:
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
             (exe      nil)
             (actual   nil)
             (errored  nil))
        (format t "~%=== ~a ===~%" base)
        (handler-case
            (progn
              (setf exe
                    (build-scheme-ast
                     (with-open-file (s path) (read s))
                     :name base
                     :build-dir build-dir
                     :runtime-c runtime-c
                     :keep-temp-files t
                     :trace trace))
              ;; Run the executable; capture exit code.
              (setf actual
                    (nth-value 2
                      (uiop:run-program (list (format nil "./~a" exe))
                                        :ignore-error-status t))))
          (error (e)
            (setf errored t)
            (format t "Compilation/Run error: ~A~%" e)))
        (let ((passed (and (not errored) (or (null expected) (= expected actual)))))
          (if passed
              (format t "  expected=~A  actual=~A  => PASS~%" expected actual)
              (format t "  expected=~A  actual=~A  => FAIL~%" expected actual))
          (push (list :file base :expected expected :actual actual :pass passed)
                results))))
    (let* ((passed-count (count-if (lambda (r) (getf r :pass)) results))
           (total-count  (length results)))
      (format t "~%--- ~D/~D Scheme examples passed ---~%" passed-count total-count)
      results)))
