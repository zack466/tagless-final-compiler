# Language Description: Scheme

Source code: `src/scheme.lisp`

## Features

This language is intended to be a minimal Scheme-like language that compiles to the Blub intermediate language.

The language supports:
- global definitions
- lambdas
- arithmetic/logical primitives
- closures

## Grammar

The grammar can be written in our custom grammar language as:

```lisp
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
```

## Example

The following Scheme function and Blub AST should be the same:

```scheme
(define (fact n) (if (< n 1) 1 (* n (fact (- n 1)))))
```

```lisp
(:define fact
  (:lambda (n)
    (:if (:le (:var n) 1)
         1
         (:mul (:var n) (:apply (:var fact) (:sub (:var n) 1))))))
```

## Interpreters

Compiler passes:
1. Rename variables (uniquify)
2. Box variables (convert assignments)
3. Convert closures
4. Remove complex operands
5. Explicate control (normalize let forms)
6. Flatten nested let/if forms, lower primtiives to function calls
7. Convert to blub (adding in prelude functions, boxing literals, sanitizing symbols, etc)
