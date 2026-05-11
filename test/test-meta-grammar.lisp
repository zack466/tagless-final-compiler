(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

(defsuite "meta grammar"
  (deftest "validates trivial rule"
    (validate-grammar '((:foo (identifier)))))

  (deftest "validates empty rule"
    (validate-grammar '((:break))))

  (deftest "validates option"
    (validate-grammar '((:foo (option :a :b)) (:a) (:b))))

  (deftest "validates nested combinators"
    (validate-grammar '((:foo (repeat0 (option (literal) :bar)))
                        (:bar (identifier)))))

  (deftest "validates implicit sequence"
    (validate-grammar '((:foo :a (identifier) (maybe :b))
                        (:a) (:b))))

  (deftest "validates dispatch"
    (validate-grammar '((:stmt (dispatch (option :a :b)))
                        (:a) (:b))))

  (deftest "validates keyword combinator"
    (validate-grammar '((:foo (keyword :hello)))))

  (deftest "validates blub grammar"
    (validate-grammar *blub-grammar*))

  (deftest "validates grammar grammar"
    (validate-grammar *grammar-grammar*)))
