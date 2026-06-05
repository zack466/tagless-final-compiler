# CS 81 Final Report - Tagless Compiler

## Introduction

The original motivation for this project was rather lofty - I wanted a "different" way of writing compilers as to make it easier to perform complicated optimizations and implement programming language features in an orthogonal manner.
I also imagined a sort of meta-programming aspect in which I'm using a language to define how the language itself is intended to be compiled, and I wanted this to be key to combining different language features in an extensible and modular way.

I figured the tagless final style of interpreters by Oleg Kiselyov was a good match for my project, as it provided a framework for program transformations that I thought was relevant and useful.
For reference, the idea of tagless final is to basically avoid "tagging" ASTs as is conventionally done in compilers, especially those implemented in languages like OCaml and Haskell.

## What is Tagless Final?

For example, the AST for a conventional scheme-like language in OCaml might look like:
```ocaml
type exp =
  | Bool    of bool
  | Int     of int
  | Var     of string
  | Set     of string * exp
  | Begin   of exp list * exp
  | If      of exp * exp * exp
  | And     of exp * exp
  | Or      of exp * exp
  | While   of exp * exp
  | Print   of exp
```

And you could imagine that any transformation on this type of AST would require a recursive function that may produce an AST of a different type.
This can lead to the [expression problem](https://en.wikipedia.org/wiki/Expression_problem), which essentially results in code bloat because every function handling the `exp` type must match on the type and provide some code to handle each possibility.
If you add a single extra type to the `exp` type, then you'll need to modify every single place an `exp` variable is matched on.

This especially hurts if you want to define a large number of small passes which modify an AST and lower it repeatedly to a slightly lower-level language, in the same manner as the [nanopass framework](https://nanopass.org/) and in textbooks like [Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation) (EOC).
The "tagged" approach forces you to define many nearly-identical AST types repeatedly and provide boilerplate to convert from each AST type to the next.
For example, here is a compiler pass from CS 164 (the "uncover get" pass of EOC) whose purpose is to mark variables that are modified by a `set!` form and ensure that these are accessed by reference using a `get!`.

```ocaml
(* uncover_get.ml *)
open Types
open Lfun_ref_alloc_get
module L = Lfun_ref_alloc
module S = VarSet

let rec concat_vars exp_lst =
  List.fold_left
    (fun var_set exp -> S.union var_set (collect_set_vars exp))
    S.empty
    exp_lst
and collect_set_vars (e : L.exp) : VarSet.t =
  match e with
  | Void
  | Bool _
  | Int _
  | Var _
  | Collect _
  | Allocate _
  | GlobalVal _
  | VecLen _
  | VecRef _
  | FunRef _
  | VecSet _ -> S.empty
  | Prim (_, exp_lst) -> concat_vars exp_lst
  | SetBang (var, exp) -> S.union (S.singleton var) (collect_set_vars exp)
  | Begin (exp_lst, body_exp) -> concat_vars (body_exp :: exp_lst)
  | If (e1, e2, e3) -> concat_vars [ e1; e2; e3 ]
  | While (e1, e2) | Let (_, e1, e2) -> concat_vars [ e1; e2 ]
  | Apply (a, b) -> S.union (collect_set_vars a) (concat_vars b)
;;

let rec uncover_get_exp (s : S.t) (e : L.exp) : exp =
  match e with
  | Void -> Void
  | Bool b -> Bool b
  | Int i -> Int i
  | Collect i -> Collect i
  | Allocate (i, t) -> Allocate (i, t)
  | GlobalVal v -> GlobalVal v
  | FunRef (a, b) -> FunRef (a, b)
  | Var v -> if S.mem v s then GetBang v else Var v
  | Prim (op, exp_lst) -> Prim (op, List.map (uncover_get_exp s) exp_lst)
  | SetBang (var, exp) -> SetBang (var, uncover_get_exp s exp)
  | Begin (exp_lst, body_exp) ->
    Begin (List.map (uncover_get_exp s) exp_lst, uncover_get_exp s body_exp)
  | If (e1, e2, e3) ->
    If (uncover_get_exp s e1, uncover_get_exp s e2, uncover_get_exp s e3)
  | While (e1, e2) -> While (uncover_get_exp s e1, uncover_get_exp s e2)
  | Let (var, e1, e2) -> Let (var, uncover_get_exp s e1, uncover_get_exp s e2)
  | VecSet (e1, i, e2) -> VecSet (uncover_get_exp s e1, i, uncover_get_exp s e2)
  | VecRef (e, i) -> VecRef (uncover_get_exp s e, i)
  | VecLen e -> VecLen (uncover_get_exp s e)
  | Apply (a, b) -> Apply (uncover_get_exp s a, List.map (uncover_get_exp s) b)
;;

let uncover_get_def (s : S.t) (ldef : L.def) : def =
  let (L.Def (name, { args; ret; body })) = ldef in
  let body' = uncover_get_exp s body in
  Def (name, { args; ret; body = body' })
;;

let uncover_get (L.Program ds) =
  let (set_vars : S.t) =
    List.fold_left
      (fun (s : S.t) (d : L.def) ->
        let (L.Def (_, fc)) = d in
        S.union s (collect_set_vars fc.body))
      S.empty
      ds
  in
  Program (List.map (uncover_get_def set_vars) ds)
;;
```

As you can see, the majority of the code is just to recurse down the AST to find the forms you're looking for, and then there is a small amount of logic that is used to combine the state recursively on the way back up, modifying the AST as necessary.
Not to mention, there are another two `.ml` files that define the two nearly-identical AST types for static type checking.

```ocaml
(* lfun_ref_alloc_get.ml *)
open Sexplib.Std
open Types

type exp =
  | Void
  | Bool      of bool
  | Int       of int
  | Var       of var
  | FunRef    of label * int
  | Prim      of core_op * exp list
  | SetBang   of var * exp
  | GetBang   of var                (* does not exist in type L.exp *)
  | Begin     of exp list * exp
  | If        of exp * exp * exp
  | While     of exp * exp
  | Let       of var * exp * exp
  | Collect   of int
  | Allocate  of int * ty
  | GlobalVal of var
  | VecLen    of exp
  | VecRef    of exp * int
  | VecSet    of exp * int * exp
  | Apply     of exp * exp list
[@@deriving sexp]

type def = Def of label * fun_contents
[@@deriving sexp]

and fun_contents =
  {
    args : (var * ty) list;
    ret  : ty;
    body : exp;
  }
[@@deriving sexp]

type program = Program of def list
[@@deriving sexp]
```

In total, excluding support libraries, it's about 200 lines to implement a pass that can be described in about 300 words (yes, I checked, EOC pg 89).

What the tagless final approach does instead is treat the AST as a structure that kind of just exists, and you instead define interpreters which assign meanings to different elements in the AST, similarly to the Free Monad.
Imagine I gave you the expression:

```
(If (LessThan 2 (Mul 2 1)) (Add 0 2) (Square (Mul 2 1)))
```

Then, instead of treating this like a recursive data structure, we can imagine assigning the following definitions to each AST node:

```haskell
Mul x y       = x * y
Add x y       = x + y
Square x      = x * x
If  x y z     = if x then y else z
LessThan x y  = x < y
```

We're not defining recursive functions that act on our AST.
Instead, we're *giving definitions* to the different forms in our AST, and the act of "interpreting" this AST simply results from the substitution of constructors to functions, giving:

```
if 2 < 2 * 1 then (0 + 2) else (2 * 1) * (2 * 1)
=> 4
```

However, our interpreters don't have to necessarily evaluate these structures.
Imagine instead a program transformation defined as:

```
Mul x y       = if x == 1 then y else if y == 1 then x else Mul x y
Add x y       = if x == 0 then y else if y == 0 then x else Add x y
```

Interpreting the original expression using this set of rules would allow us to elide additive/multiplicative identities, resulting in a much simpler expression:

```
if 2 < 2 then 2 else 2 * 2
=> 4
```

So we have essentially implemented a very simple version of constant folding, directly on our AST structure, in just 2 lines of (psuedo-)code.
What I find appealing about this structure is that it allows us to write program transformations in a more concise manner, while still enabling complicated behaviors.
We also didn't have to explicitly define an "AST" type nor the input/output types, though static typing is possible.

Oleg Kiselyov has also experimented with using tagless interpreters for metaprogramming, in which he uses OCaml to generate OCaml code with the tagless final framework to perform symbolic transformations and optimizations that would be very difficult otherwise.
This system is very similar to Lisp macros, which can be used to implement complex program transformations.
For example, the [CL-CONT](https://cl-cont.common-lisp.dev/) library includes a macro which converts a subset of Common Lisp programs into continuation-passing style just to implement delimited continuations using `call/cc`, which is not part of the original language.
All of this is to say, I wanted to combine all of these ideas and implement a compiler using just the tagless final style in a way that is also similar to macro-expansion.
I figured this alternative paradigm might be worth looking into.

## Common Lisp

I chose Common Lisp both out of interest (I wanted to get more experience with it) and because of its synergy with the tagless-final style.
In Common Lisp, it's common to use lists to pass around data, including complicated structures like ASTs, so I figured I could use this as the base data structure for my compiler.
I created an interpreter framework which automatically performs recursive tree-walking through an AST and applies definitions based on the type of node (which I denoted using keyword symbols).
For example, you can define a simple interpreter with three "handlers" as follows:

```lisp
(defvar arith-eval (make-interpreter))

(def-op arith-eval (:add a b) (+ (recurse a) (recurse b)))
(def-op arith-eval (:mul a b) (* (recurse a) (recurse b)))
(def-op arith-eval (:inc a)   (1+ (recurse a)))

(lower arith-eval '(:mul (:add 1 2) (:inc 5))) ; -> 18
```

One choice I made here was to make recursion explicit.
While this isn't necessary, as seen above, I wanted to increase flexibility in the case that I did not want a depth-first traversal.
Furthermore, I added an option for the interpreter to automatically recurse on unknown keywords, so essentially I removed all of the boilerplate for AST recursion and only need to write handlers for the parts of the AST I actually cared about.

I also made use of dynamic variables to keep track of state within a single compiler pass.
Dynamic variables in Common Lisp are basically delimited globals, which doesn't sound great, but I basically used them me to pass values in/out of handlers in a somewhat disciplined way.
So this essentially provides the same behavior as recursive tree-walking functions that pass state in/out, just without the syntactic overhead and boilerplate.

For example, here is a snippet of code that identifies variables which need to be boxed (similarly to the "uncover get" pass from above), which turns out to be extremely simple.

```lisp

(defparameter *scheme-2a* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-2A"))

(defparameter *s2a-captured-vars* (fset:empty-set) "Variables captured by an enclosing lambda")
(defparameter *s2a-vars-to-box*   (fset:empty-set) "Accumulator: variables needing boxing")

;; Keep track of captured parameters in *s2a-captured-vars*
(def-op *scheme-2a* (:lambda params &rest body)
  (let ((*s2a-captured-vars* (fset:convert 'fset:set params)))
    (append (list :lambda params) (mapappend #'recurse-splice body))))

;; If variable on lhs of a set! form is not captured, add to set of
;; variables which need to be boxed.
(def-op *scheme-2a* (:set! var form)
  (when (and (not (fset:contains? *s2a-captured-vars* var))
             (not (fset:contains? *scheme-globals* var)))
    (setf *s2a-vars-to-box* (fset:with *s2a-vars-to-box* var)))
  (list :set! var (recurse form)))
```

The, the second half of this pass can perform the actual rewriting, modifying reads/writes to boxed variables by adding references/dereferences.

```lisp
(defparameter *scheme-2b* (make-interpreter :on-unknown :recurse
                                            :readable-name "SCHEME-2B"))

(defun s2b-boxed-p (var) (fset:contains? *s2a-vars-to-box* var))

;; Convert boxed (:var v) to (:deref (:var v))
(def-op *scheme-2b* (:var var)
  (if (s2b-boxed-p var)
      (list :deref (list :var var))
      (list :var var)))

;; Convert boxed (:set! v x) to (:set-ref! v x)
(def-op *scheme-2b* (:set! var form)
  (if (s2b-boxed-p var)
      (list :set-ref! var (recurse form))
      (list :set! var (recurse form))))

;; Convert boxed variables v into (:make-ref v) forms within let bindings
(def-op *scheme-2b* (:let bindings &rest body)
  (let ((bindings* (mapcar (lambda (binding)
                             (destructuring-bind (name value) binding
                               (if (s2b-boxed-p name)
                                   (list name (list :make-ref (recurse value)))
                                   (list name (recurse value)))))
                           bindings)))
    (append (list :let bindings*) (mapappend #'recurse-splice body))))

;; Convert boxed parameters v into (:make-ref v) forms within lambda formsc
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

```

This definitely isn't the cleanest code, but it implements the pass without fail and in about a third of the lines required in the OCaml "uncover get" compiler pass, which does something very similar.
Now in order to implement programs that can actually run on my computer (rather than being interpreted), I needed to compile my languages down to assembly.

## Language: QBE

To avoid having to implement a bunch of tedious ABI stuff and also keep this compiler more cross-platform, I chose to use [QBE](https://c9x.me/compile/) as the backend.
QBE is very minimal but still provides all the features you would expect from an SSA-based intermediate representation (arithmetic/bitwise instructions, memory allocation, comparisons, conversions, function calls, branching, etc).
I implemented what is effectively a pretty-printer which takes an AST representation of QBE and outputs it as a string which can then be assembled by the `qbe` program.
This was pretty much just a 1-to-1 mapping.

For example:
```
(:MODULE
  (:FUNCTION (:GLOBAL "qbe_main") :EXPORT
    :W
    NIL
    (:BLOCK
      (:LABEL #:|start_189|)
      (:RET 42))
    (:BLOCK
      (:LABEL #:|dead_190|)
      (:RET 0))))

=>

export function w $qbe_main() {
@start_189
        ret 42
@dead_190
        ret 0
}

=>

.text
.balign 4
.globl _qbe_main
_qbe_main:
	hint	#34
	stp	x29, x30, [sp, -16]!
	mov	x29, sp
	mov	w0, #42
	ldp	x29, x30, [sp], 16
	ret
/* end function qbe_main */

```

## Language: Blub

Next, I wanted to implement more of a C-like language which could be translated, rather straightforwardly, to my QBE language.
To do this, I implemented the following compiler passes:
0. Desugaring
1. Variable renaming (uniquify)
2. Struct layout resolution (computing struct field offsets)
3. Typechecking
4. Remove complex operands (convert nested expressions to three-address code)
5. Lower to QBE (mostly a 1-to-1 translation)

Most of the transformations are basically what you would expect.
Structs are translated directly into QBE aggregate types, and nested expressions are all flattened out into individual operations on temporaries.
Variables and pointers are all handled using QBE's load/store instructions, and the QBE `call` instruction is compatible with the C ABI.

For example, this C code can be represented in Blub and then converted to QBE like so:

```
struct Point { int x; int y; };

int sum_fields(struct Point *p) {
  return p->x + p->y;
}

int qbe_main() {
  struct Point pt;
  pt.x = 3;
  pt.y = 4;
  return sum_fields(&pt);   /* = 7 */
}

=>

(:defstruct point ((:type :i32) x) ((:type :i32) y))

(:function (:type :i32) sum-fields
  ((:type (:pointer (:type (:struct point)))) p)
  (:block
    (:return (:add (:-> (:var p) x)
                   (:-> (:var p) y)))))

(:function (:type :i32) qbe_main
  (:block
    (:declare (:type (:struct point)) pt)
    (:set (:. (:var pt) x) 3)
    (:set (:. (:var pt) y) 4)
    (:return (:call sum-fields (:addr-of (:var pt))))))

=>

type :point = { w, w }

export function w $sum_fields(l %P_52) {
@start_51
        %P.ptr_53 =l alloc8 8
        storel %P_52, %P.ptr_53
        %t_48.ptr_54 =l alloc4 4
        %v_55 =l loadl %P.ptr_53
        %X.fval_56 =w loadsw %v_55
        storew %X.fval_56, %t_48.ptr_54
        %t_49.ptr_57 =l alloc4 4
        %v_58 =l loadl %P.ptr_53
        %Y.fptr_59 =l add %v_58, 4
        %Y.fval_60 =w loadsw %Y.fptr_59
        storew %Y.fval_60, %t_49.ptr_57
        %v_61 =w loadsw %t_48.ptr_54
        %v_62 =w loadsw %t_49.ptr_57
        %ADD_63 =w add %v_61, %v_62
        ret %ADD_63
@dead_64
        ret 0
}

export function w $qbe_main() {
@start_65
        %PT.ptr_66 =l alloc4 8
        storew 3, %PT.ptr_66
        %fptr_67 =l add %PT.ptr_66, 4
        storew 4, %fptr_67
        %t_50.ptr_68 =l alloc8 8
        storel %PT.ptr_66, %t_50.ptr_68
        %v_69 =l loadl %t_50.ptr_68
        %call_70 =w call $sum_fields(l %v_69)
        ret %call_70
@dead_71
        ret 0
}

=>

.text
.balign 4
.globl _sum_fields
_sum_fields:
	hint	#34
	stp	x29, x30, [sp, -16]!
	mov	x29, sp
	mov	x1, x0
	ldr	w0, [x1]
	mov	x2, #4
	add	x1, x1, x2
	ldr	w1, [x1]
	add	w0, w0, w1
	ldp	x29, x30, [sp], 16
	ret
/* end function sum_fields */

.text
.balign 4
.globl _qbe_main
_qbe_main:
	hint	#34
	stp	x29, x30, [sp, -32]!
	mov	x29, sp
	add	x1, x29, #24
	mov	w0, #3
	str	w0, [x1]
	mov	x1, #4
	add	x0, x29, #24
	add	x1, x0, x1
	mov	w0, #4
	str	w0, [x1]
	add	x0, x29, #24
	bl	_sum_fields
	ldp	x29, x30, [sp], 32
	ret
/* end function qbe_main */

```

It definitely doesn't produce the most efficient code (in fact, it's roughly on par with `clang -O0`), but it works.
The goal was basically to make a C-like language which can now be optimized as part of my framework and can also be used as the target for a higher-level language with more complex language features.
The syntax also isn't great, but it should be pretty trivial to attach a parser frontend that converts normal C-like syntax to my s-expression language before compiling it like normal.

## Language: Scheme

Now, I wanted to experiment with writing a language which compiles down to Blub.
I chose Scheme, which definitely has language semantics which are non-trivially different than C.
But luckily, since Scheme defers things like type-checking to runtime (for the most part) and since my Blub language supports more complex operands than QBE, I had more leeway in the compiler structure.
Basically, I didn't have to deal with anything like converting Scheme code to the QBE SSA form, since the Blub language already does this.
Furthermore, Scheme primitives can be trivially mapped to C functions which perform typechecking at runtime and can manipulate boxed values very easily.
Rather, the main challenge was closure conversion and flattening the structure of the code (everything else could be mapped roughly 1-to-1).

Compiler Passes:
0. Desugaring
1. Rename variables (uniquify)
2. Box variables (convert assignments)
3. Convert closures
4. Remove complex operands
5. Explicate control (normalize let forms)
6. Flatten nested let/if forms, lower primtiives to function calls
7. Convert to blub (adding in prelude functions, boxing literals, sanitizing symbols, etc)

Excluding helper functions, the translation process looks something like:
```
;; Basic Scheme arithmetic test
; expected exit code: 25
(:module
  (:define main
    (:lambda ()
      (:mul (:add 2 3) 5))))

=>

(:FUNCTION (:TYPE :U64) #:|lambda_884|
  ((:TYPE :U64) #:|closure_885|)
  (:BLOCK
    (:DECLARE (:TYPE :U64) #:|tmp_886| (:CALL _ADD 32 48))
    (:RETURN (:CALL _MUL (:VAR #:|tmp_886|) 80))))

(:GLOBAL (:TYPE :U64) _SCHEME_MAIN 0)

(:FUNCTION (:TYPE :U64) _SCHEME_INIT_GLOBALS
  (:BLOCK
    (:SET
      _SCHEME_MAIN
      (:CALL
        _MAKE_CLOSURE
        (:CAST (:TYPE :U64) (:FN-PTR #:|lambda_884|))
        (:CAST (:TYPE :U64) 0)
        (:VARARGS)))
    (:RETURN 0)))

(:FUNCTION (:TYPE :I32) QBE_MAIN
  (:BLOCK
    (:DECLARE (:TYPE :U64) _INIT (:CALL _SCHEME_INIT_GLOBALS))
    (:DECLARE (:TYPE :U64) M (:VAR _SCHEME_MAIN))
    (:DECLARE (:TYPE :U64) FNP (:CALL _CLOSURE_FN (:VAR M)))
    (:DECLARE (:TYPE :U64) ENV (:CALL _CLOSURE_ENV (:VAR M)))
    (:DECLARE (:TYPE :U64) R
      (:CALL (:CAST (:TYPE (:FN (:TYPE :U64) (:TYPE :U64))) (:VAR FNP)) (:VAR ENV)))
    (:RETURN (:CAST (:TYPE :I32) (:SHR (:VAR R) 4)))))

=>

to QBE and then assembly as expected
```

As you can see, the main lambda is lifted into a closure before being called in the driver `qbe_main` function.
While not structly necessary in this case, this is required to handle free variables and `set!` properly when nesting `let` and `lambda` forms.

## Lessons / Conclusions

I would say that I vastly underestimated the complexity of compilers, even with these relatively simple passes.
I tried to implement the bare minimum to get everything working, but there is just a lot of logic necessary to implement even the simplest forms of type-checking, flattening, and lowering.
And while I wanted to make a "different" sort of compiler, most of my passes are basically re-implementations of existing passes that exist in EOC without much modification.
However, I do think that my interpreter framework is a decent first start for a compiler "library".

The majority of the time spent on this project was getting the compilers working, and I didn't have much time to work on the "cooler" language features I had wanted to initially.
But I think this was inevitable, as I don't think there's a high-level IR anyone uses for developing interesting programming languages - it seems everyone just rolls their own IR before dropping it into LLVM or something.
So I do think that I can definitely build on the infrastructure I've created and work with interesting compiler passes that reduce down to my Blub or Scheme languages.
I do think that maybe trying to implement C/Scheme was a slight mis-step, as I don't they are very good intermediate representations.
I think C is just too fully-featured, which explains why languages like Haskell instead use C--, a super pared down version of C, as an IR.
Or maybe its more that different languages just have their own distinct semantics which don't play well with each other.
It's possible some ML-style language would work better as a general-purpose IR, which could provide static typing and be easier to optimize.

## Future Work

Especially since I had wanted to work on compiler optimizations initially, I think there are tons of optimizations that can be applied to my languages.
The current implementation of Blub and Scheme are super naive, without even the most basic optimizations like constant folding or inlining.
And QBE provides a more minimal set of optimizations, so it's more up to me to perform high-level optimizations on my own intermediate representations.
I think optimizations are especially key to implementing more complicated programming language features in a way that's not insanely slow.
I'm especially inspired by Haskell, which performs most of its optimizations on a typed-lambda-calculus-based IR and is able to generate very fast code even though the language is so high-level.
And I'm also interested in projects like OxCaml and Coalton, which are intended to squeeze as much performance as possible out high-level languages (OCaml and Common Lisp, respectively).
I want to see what types of IRs they use and how they achieve their high performance, and implement similar compiler passes in this project.
