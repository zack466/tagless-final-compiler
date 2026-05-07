# Language Description: Blub

Source code: `src/blub.lisp`

## Features

This language is intended to be a minimal C-like language that compiles to the QBE intermediate language (see QBE language).
The language supports:
- fixed size, stack-allocated variables with standard types (char, int, float, double, pointer)
- can take address of variables and dereference pointers
- standard imperative control flow (if/else, while/break/continue)
- function definitions (compatible with C ABI)
- global variables
- standard arithmetic operators, comparisons, logic, bitwise operations
- structs (uses QBE aggregate types)
- varargs
- TODO: standard library

## Grammar

The grammar can be written in our custom grammar language as:

```lisp
((:module
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
        (:assign i (:mul (:var i) (:var j)))
        (:assign j (:sub (:var j) 1))))
    (:return (:var i))))
```

## Interpreters

;; Compiler passes (in no particular order)
;; - typecheck standard operators, function calls, pointers, etc
;; - resolve all struct definitions, determine total size, plus size and offset of each field
;; - cps transformation from nested statements into SSA
