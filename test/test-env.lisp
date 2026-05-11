(in-package #:tagless-compiler)

(define-pass-context *test-ctx* :doc "A context for testing.")

(defsuite "env/context"
  (deftest "basic bind and lookup"
    (with-empty-scope (*test-ctx*)
      (with-scope (*test-ctx*)
        (env-bind *test-ctx* 'a 10)
        (env-bind *test-ctx* 'b 20)
        (multiple-value-bind (val present) (env-lookup *test-ctx* 'a)
          (check val 10)
          (check-true present))
        (multiple-value-bind (val present) (env-lookup *test-ctx* 'b)
          (check val 20)
          (check-true present))
        (multiple-value-bind (val present) (env-lookup *test-ctx* 'c)
          (check val nil)
          (check-false present)))))

  (deftest "dynamic shadowing"
    (with-empty-scope (*test-ctx*)
      (with-scope (*test-ctx*)
        (env-bind *test-ctx* 'x 1)
        (with-scope (*test-ctx*)
          (check (env-lookup *test-ctx* 'x) 1)
          (env-bind *test-ctx* 'x 2)
          (check (env-lookup *test-ctx* 'x) 2)
          (env-bind *test-ctx* 'y 3)
          (check (env-lookup *test-ctx* 'y) 3))
        (check (env-lookup *test-ctx* 'x) 1)
        (multiple-value-bind (val present) (env-lookup *test-ctx* 'y)
          (declare (ignore val))
          (check-false present)))))

  (deftest "empty scope has no bindings"
    (with-empty-scope (*test-ctx*)
      (multiple-value-bind (val present) (env-lookup *test-ctx* 'missing)
        (declare (ignore val))
        (check-false present))))

  (deftest "with-scope without with-empty-scope signals error"
    (check-error (with-scope (*test-ctx*) t))))
