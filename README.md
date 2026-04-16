# A Tagless Final Compiler

## Building

I apologize in advance for using git submodules.
```
# clone this repo using:
git clone --recurse-submodules https://github.com/zack466/tagless-final-compiler.git
```

If you have nix, just run `nix develop` and everything should work fine.
For non-nix users, you'll have to `make` each dependency manually and add the executables to your PATH.

## Docs

Related reference material can be found in `docs/`.

## Running

I would recommend running `sbcl` and then loading the dev environment as follows:
```
$ sbcl
> (load "dev.lisp")
> (in-package #:tagless-compiler)
> (lower arith-eval  '(:mul (:add 1 2) (:inc 5))) ; -> 18
> (lower string-repr '(:mul (:add 1 2) (:inc 5))) ; -> "(Mul (Add 1 2) (Inc 5))"
```

## Libraries
- [fset](https://gitlab.common-lisp.net/fset/fset/-/wikis/home)?
- [alexandria](https://alexandria.common-lisp.dev/)?
