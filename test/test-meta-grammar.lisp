(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

;;; Tests for the meta-grammar. Assumes deftest, *tests-run*, etc. from
;;; grammar-tests.lisp are already loaded.

(defun run-meta-tests ()

  (deftest meta-validates-trivial-rule
    (validate-grammar '((:foo (identifier)))))

  (deftest meta-validates-empty-rule
    ;; Rules with no slots, like our :break and :continue.
    (validate-grammar '((:break))))

  (deftest meta-validates-option
    (validate-grammar '((:foo (option :a :b)) (:a) (:b))))

  (deftest meta-validates-nested-combinators
    (validate-grammar '((:foo (repeat0 (option (literal) :bar)))
                        (:bar (identifier)))))

  (deftest meta-validates-implicit-sequence
    (validate-grammar '((:foo :a (identifier) (maybe :b))
                        (:a) (:b))))

  (deftest meta-validates-dispatch
    (validate-grammar '((:stmt (dispatch (option :a :b)))
                        (:a) (:b))))

  (deftest meta-validates-keyword-combinator
    (validate-grammar '((:foo (keyword :hello)))))

  (deftest meta-validates-blub-grammar
    ;; The full toy language grammar should be self-consistent.
    (validate-grammar *blub-grammar*))

  (deftest meta-validates-itself
    ;; The grammar grammar should describe its own shape correctly.
    (validate-grammar *grammar-grammar*)))

(run-meta-tests)
