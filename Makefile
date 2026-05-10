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
		--eval '(asdf:load-system :tagless-compiler/tests)' \
		--eval '(sb-ext:exit)'

examples:
	rlwrap sbcl --load dev.lisp \
		--eval '(in-package #:tagless-compiler)' \
		--eval '(run-blub-examples)' \
		--eval '(sb-ext:exit)'

clean:
	rm src/*.fasl

clean-build:
	rm -rf build/

.PHONY: repl test build examples clean clean-build
