repl:
	rlwrap sbcl --load dev.lisp \
		 --eval '(in-package #:tagless-compiler)' \
		 --eval '(named-readtables:in-readtable tagless-compiler-syntax)'

test:
	rlwrap sbcl --load dev.lisp \
		--eval '(in-package #:tagless-compiler)' \
		--eval '(asdf:load-system :tagless-compiler/tests)'

.PHONY: repl test
