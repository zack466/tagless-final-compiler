repl:
	rlwrap sbcl --load dev.lisp \
		 --eval '(in-package #:tagless-compiler)' \
		 --eval '(named-readtables:in-readtable tagless-compiler-syntax)'

build:
	rlwrap sbcl --load dev.lisp \
		 --eval '(in-package #:tagless-compiler)' \
		 --eval '(named-readtables:in-readtable tagless-compiler-syntax)' \
		 --eval '(sb-ext:exit)'

test:
	rlwrap sbcl --load dev.lisp \
		--eval '(in-package #:tagless-compiler)' \
		--eval '(asdf:load-system :tagless-compiler/tests)'

clean:
	rm src/*.fasl

.PHONY: repl test build clean
