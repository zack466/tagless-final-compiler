# Language Description: Blub

Source code: `src/blub.lisp`

## Features

This language is intended to be a minimal C-like language that compiles to the QBE intermediate language (see QBE language).
The language supports:
- fixed size, stack-allocated variables with standard types (u8, i8, u32, i32, u64, i64, f32, f64)
- can take address of variables and dereference pointers
- standard imperative control flow (if/else, while/break/continue)
- function definitions (compatible with C ABI)
- function pointers
- casting
- global variables
- standard arithmetic operators, comparisons, logic, bitwise operations
- structs (uses QBE aggregate types)
- varargs
- TODO: standard library

## Grammar

The grammar can be written in our custom grammar language as:

```lisp
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
```

## Example

The following C function and Blub AST should be the same:

```c
int factorial(int j) {
  int i = 1;
  while (j > 0) {
    i = i * j;
    j = j - 1;
  }
  return i;
}
```

```lisp
(:function (:type :int) factorial (:args ((:type :int) j))
  (:block
    (:declare (:type :int) i 1)
    (:while (:ge (:var j) 0)
      (:block
        (:set i (:mul (:var i) (:var j)))
        (:set j (:sub (:var j) 1))))
    (:return (:var i))))
```

## Interpreters

;; Compiler passes (in no particular order)
;; - typecheck standard operators, function calls, pointers, etc
;; - resolve all struct definitions, determine total size, plus size and offset of each field
;; - cps transformation from nested statements into SSA
