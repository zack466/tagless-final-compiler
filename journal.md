# 3/31/26

The challenge: combining expressivity in programming languages with high performance

Solution: define a meta-language that can define its own constructs and how to compile them (so obviously it has to be a lisp)

There is a series of sub-languages, where each one compiles to the one below it, going down to assembly.
But the meta-language allows you to program in any of these languages, all in the same program.

Original motivation: I noticed that in many languages, optimization come from writing your code in a very specific way in order to enable massive compiler optimizations.
However, there can be issues where, since the compiler is a big black box, the optimizations might not occur, which can be frustrating.

Examples:
- Stream fusion in Haskell
  - needs a plugin to ensure the needed compiler transformations occur: https://hackage.haskell.org/package/fusion-plugin
  - book: algorithm design with haskell
- C++
  - compile-time dispatch + inlining (event handlers), basically devirtualization
  - zero-copy operations that take lambdas (effectively turns into continuation passing style)

What if we just defined each of these as a language feature that is separate, and able to be combined orthogonally within the same program?

Haskell has compiler plugins and rewriting rules, but this doesn't allow adding entirely different features to the language (at least i don't think).
But I do like how Haskell compiles to a lambda-based IR rather than something like SSA, enables very interesting types of optimizations.
What if I could combine SSA and basic block optimizations separately, or in parallel, for different parts of my language?

Or in general, what if my language could give me the features to implement entire sub-languages within it that get compiled efficiently?
So it's a language to write compilers in.
If we have control over the assembly itself at a low level, we can write programs with all sorts of different paradigms, like functional reactive programming, or actors, or coroutines, etc, all in the same language, but swapping out the runtime environment.

Related works:
- Multi-stage programming
  - [MetaOCaml](https://okmij.org/ftp/ML/MetaOCaml.html)
    - https://okmij.org/ftp/ML/MetaOCaml.html
    - https://okmij.org/ftp/tagless-final/cookbook.html#simple-staging
    - https://okmij.org/ftp/meta-programming/translation.pdf
    - https://okmij.org/ftp/meta-programming/strymonas.pdf
    - https://okmij.org/ftp/meta-programming/tutorial/
    - https://okmij.org/ftp/meta-programming/design-10.pdf
    - https://okmij.org/ftp/tagless-final/index.html
    - https://okmij.org/ftp/tagless-final/course/optimizations.html
    - https://okmij.org/ftp/tagless-final/course/lecture.pdf
  - [Terra](https://terralang.org/)
  - [NanoPass](https://nanopass.org/index.html)
    - [Thesis](https://andykeep.com/pubs/dissertation.pdf)
  - [Delite](https://stanford-ppl.github.io/Delite/index.html)
- 3-lisp
  - https://github.com/nikitadanilov/3-lisp
  - https://news.ycombinator.com/item?id=32614795
  - "reflective tower"
- Common Lisp's MOP?
- solves "colored functions"?

TODO (for next meeting):
- dig deeper into a specific idea, 81-sized project (err on side of less work, not too ambitious)
- print out schedule change form to get signed

# 4/4/26

Name Ideas:
- Meta-Compiler Protocol (like MOP)
- A Tagless-Final Compiler
- A Symantic Compiler

Core Ideas:
- nanopass
  - small compiler passes
- MetaOCaml:
  - motivated by interpreters which "specialize" code, basically lisp macros combined with optimizing passes that operate on code as a data structure
  - staged programming - metaprogramming with macros
  - tagless final style - code as data
  - DSLs for high performance, for things like filtering, linear algebra, image processing, etc
  - [Modular, composable, typed optimizations in the tagless-final style](https://okmij.org/ftp/tagless-final/course/optimizations.html)
- CLOS
  - metaobject protocol
  - multiple dispatch
  - generic and extensible

Goals:
- be able to define a language and how to compile it in a unified, extensible manner
- be able to define every compiler pass in the language, all the way from a high level language down to assembly
  - be able to program in this language at any level, write functions that can take code from any level and compile it down to any other level
- should be able to define new language structures and compiler passes in the language itself

Ideas:
- compile to simple IR, [Cranelift](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md) or [QBE](https://c9x.me/compile/)?

What optimizations / language features should I test / implement?
- algebraic manipulations
- loop vectorization using SIMD
- stream fusion (like strymonas)
- circuit-like language / HDL
- algebraic data types
- effects

What should the language look like?
It should define how to compile different types of terms, all the way down to some IR which can then be injected into some runtime environment and executed.

Steps:
- First, define the lowest level of the language, which should map 1-to-1 with our IR of choice.
  - our interpreter should be able to take this language, convert it into IR, then lower it to assembly and run it.
- Then, define c-like language features using our compiler passes
- And then implement some nontrivial program transformations in a way that is orthogonal or compiles to our existing language features so everything gets compiled down

(defpass ())

Languages:
- Base: 1-to-1 with IR (SSA)
- Blub: weakly-typed, C style language, compiles to Base
- Lambda: weakly-typed lambda calculus, compiles to Base

Q: How to share variables/expressions between the two languages?

+ runtime environment if necessary (most likely will just call into some entrypoint in the assembly)

More features can be implemented on top of these languages, but if I can have interop between a c-style language a pure functional language, then I think it will be very interesting.

# 4/9/26

Proposed Schedule:
Week 2: Define the lowest level of the system that takes code at the Base level, interprets it as assembly, and links it into a simple runtime.
Weeks 3-5: Define a tagless-final-style interpreter that takes code written in some higher-abstraction language and converts it to the Base language.
  - Blub c-style language
  - Lambda calculus-based language
  - this could also just be the whole project
Weeks 6-8: Define special language features in the interpreter by defining local/global transformations that reduce to the blub language, lambda calculus languages, or Base language.
  - loop vectorization (blub)
  - algebraic simplifications (lambda calculus)
  - circuit-like language / HDL (mix of both)
  - stream fusion
Weeks 9-10: Choose a more complicated language feature and implement that in a way that is modular / composable and efficient down to the assembly
  - fibers / coroutines / async
  - effect handlers

TODO
- explicitly write out plan
- put referenced papers into a docs/ directory
- send link to repo when it's set up
