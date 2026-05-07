(in-package #:tagless-compiler)

(define-pass-context *test-ctx* :doc "A context for testing.")

;; A helper macro to wrap the tests nicely without repeating boilerplate
(defmacro deftest (name &body body)
  `(defun ,(intern (symbol-name name) (find-package '#:tagless-compiler)) ()
     ,@body))

(deftest context-basic-bind-and-lookup
  (with-empty-scope (*test-ctx*)
    (with-scope (*test-ctx*)
      (env-bind *test-ctx* 'a 10)
      (env-bind *test-ctx* 'b 20)
      (multiple-value-bind (val present) (env-lookup *test-ctx* 'a)
        (check "context-basic-bind-and-lookup val A" val 10)
        (check "context-basic-bind-and-lookup present A" present t))
      (multiple-value-bind (val present) (env-lookup *test-ctx* 'b)
        (check "context-basic-bind-and-lookup val B" val 20)
        (check "context-basic-bind-and-lookup present B" present t))
      (multiple-value-bind (val present) (env-lookup *test-ctx* 'c)
        (check "context-basic-bind-and-lookup val C" val nil)
        (check "context-basic-bind-and-lookup present C" present nil)))))

(deftest context-dynamic-shadowing
  (with-empty-scope (*test-ctx*)
    (with-scope (*test-ctx*)
      (env-bind *test-ctx* 'x 1)
      (with-scope (*test-ctx*)
        ;; Inner scope sees outer scope
        (check "context-dynamic-shadowing inner reads outer" (env-lookup *test-ctx* 'x) 1)
        ;; Inner scope can override
        (env-bind *test-ctx* 'x 2)
        (check "context-dynamic-shadowing inner overrides" (env-lookup *test-ctx* 'x) 2)
        ;; Inner scope can add
        (env-bind *test-ctx* 'y 3)
        (check "context-dynamic-shadowing inner adds" (env-lookup *test-ctx* 'y) 3))
      ;; Outer scope is unaffected by inner scope mutations
      (check "context-dynamic-shadowing outer restores X" (env-lookup *test-ctx* 'x) 1)
      (multiple-value-bind (val present) (env-lookup *test-ctx* 'y)
        (declare (ignore val))
        (check "context-dynamic-shadowing outer forgets Y" present nil)))))

(deftest context-default-initial-value
  ;; Start fresh to test the reset macro
  (with-empty-scope (*test-ctx*)
    (multiple-value-bind (val present) (env-lookup *test-ctx* 'missing)
      (declare (ignore val))
      (check "context-default-initial-value missing" present nil))))

(deftest context-unbound-error
  ;; Testing that using with-scope before with-empty-scope throws an error
  (handler-case (progn (with-scope (*test-ctx*) t)
                       (check "context-unbound-error" t nil)) ; Fail if it doesn't error
    (error () (check "context-unbound-error" t t))))

(context-basic-bind-and-lookup)
(context-dynamic-shadowing)
(context-default-initial-value)
(context-unbound-error)
