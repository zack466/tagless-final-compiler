;;;; Test &whole + match-pattern + match-cases.
;;;;
;;;; Kept in its own package because match-pattern / match-cases are
;;;; legacy "v4" constructs that don't exist in the main package.
;;;; The test framework mirrors the main harness (quiet on success).

(in-package #:tagless-compiler)

(defsuite "match-pattern: &whole"
  (deftest "&whole binds original cons"
    (clear-source-locations)
    (let* ((tn    (tag (list 'int 'x) "f" 5))
           (input (list :declare tn 0)))
      (match-pattern input (:declare (&whole pair type name) value)
        (check type 'int)
        (check name 'x)
        (check value 0)
        (check-true (eq pair tn))
        (check-true (source-loc pair))
        (check (source-loc-start-line (source-loc pair)) 5)
        '(:done))))

  (deftest "&whole with empty input binds nil"
    (clear-source-locations)
    (match-pattern '(:noop) (:noop &whole all)
      (check all nil)
      'ok))

  (deftest "&whole at top level of lambda list"
    (match-pattern '(:foo 1 2) (:foo &whole all a b)
      (check all '(1 2))
      (check a 1)
      (check b 2)
      'ok))

  (deftest "&whole + &optional"
    (match-pattern '(:f 1) (:f &whole all a &optional (b 99))
      (check all '(1))
      (check a 1)
      (check b 99)
      'ok))

  (deftest "&whole + &rest"
    (match-pattern '(:f 1 2 3 4) (:f &whole all a &rest tail)
      (check all '(1 2 3 4))
      (check a 1)
      (check tail '(2 3 4))
      'ok))

  (deftest "nested &whole preserves both cons cells"
    (clear-source-locations)
    (let* ((inner (tag (list 'b 1) "f" 9))
           (outer (tag (list 'a inner) "f" 8))
           (expr  (list :f outer 'end)))
      (match-pattern expr (:f (&whole o a (&whole i b val)) tail)
        (check-true (eq o outer))
        (check-true (eq i inner))
        (check (source-loc-start-line (source-loc o)) 8)
        (check (source-loc-start-line (source-loc i)) 9)
        (check (list a b val) '(a b 1))
        (check tail 'end)
        'ok))))

(defsuite "match-pattern: loc propagation"
  (deftest "result inherits loc from expr"
    (clear-source-locations)
    (let* ((node (tag (list :double 7) "f" 11)))
      (let ((result (match-pattern node (:double n)
                      (list :pair n n))))
        (check result '(:pair 7 7))
        (check-true (source-loc result))
        (check (source-loc-start-line (source-loc result)) 11))))

  (deftest "existing loc on result is not overwritten"
    (clear-source-locations)
    (let* ((node     (tag (list :double 7) "outer" 11))
           (existing (tag (list :preexisting) "inner" 99)))
      (let ((result (match-pattern node (:double n)
                      (declare (ignore n))
                      existing)))
        (check-true (eq result existing))
        (check (source-loc-start-line (source-loc result)) 99))))

  (deftest "mismatch on head signals"
    (handler-case
        (progn (match-pattern '(:other) (:declare a) a)
               (error "should have errored"))
      (malformed-operator-args () :ok)))

  (deftest "arity mismatch signals"
    (handler-case
        (progn (match-pattern '(:f 1 2 3) (:f a b) (list a b))
               (error "should have errored"))
      (malformed-operator-args () :ok))))

(defsuite "match-cases: dispatch and propagation"
  (deftest "first clause matches"
    (clear-source-locations)
    (let* ((expr (tag (list :add 1 2) "f" 3)))
      (let ((r (match-cases expr
                 ((:add a b) (list :sum a b))
                 ((:mul a b) (list :prod a b)))))
        (check r '(:sum 1 2))
        (check (source-loc-start-line (source-loc r)) 3))))

  (deftest "second clause matches"
    (let ((r (match-cases '(:mul 3 4)
               ((:add a b) (list :sum a b))
               ((:mul a b) (list :prod a b)))))
      (check r '(:prod 3 4))))

  (deftest "t fallback"
    (let ((r (match-cases 'just-a-symbol
               ((:add a b) (list :sum a b))
               (t :catchall))))
      (check r :catchall)))

  (deftest "no match signals"
    (handler-case
        (progn (match-cases '(:xyz)
                 ((:add a b) (declare (ignore a b)) :sum)
                 ((:mul a b) (declare (ignore a b)) :prod))
               (error "should have errored"))
      (malformed-operator-args () :ok)))

  (deftest "arity error does not fall through to t clause"
    (handler-case
        (progn (match-cases '(:add)
                 ((:add a b) (declare (ignore a b)) :sum)
                 (t :catchall))
               (error "should have errored, not fallen through"))
      (malformed-operator-args () :ok)))

  (deftest "&whole inside match-cases"
    (clear-source-locations)
    (let* ((tn   (tag (list 'int 'y) "f" 12))
           (expr (list :declare tn 5)))
      (let ((r (match-cases expr
                 ((:declare (&whole pair type name) val)
                  (declare (ignore type name val))
                  (list :got pair)))))
        (check-true (eq (second r) tn))
        (check (source-loc-start-line (source-loc (second r))) 12)))))

(defsuite "match-pattern: parser error cases"
  (deftest "&whole not first signals"
    (handler-case
        (progn (parse-lambda-list '(a &whole b))
               (error "should have errored"))
      (error () :ok)))

  (deftest "&whole without var signals"
    (handler-case
        (progn (parse-lambda-list '(&whole))
               (error "should have errored"))
      (error () :ok)))

  (deftest "&whole keyword-var signals"
    (handler-case
        (progn (parse-lambda-list '(&whole :foo a))
               (error "should have errored"))
      (error () :ok))))

(defsuite "match-pattern: realistic handler style"
  (deftest "rebuild inherits outer loc, inner cons is unchanged"
    (clear-source-locations)
    (let* ((inner-pair (tag (list 'int 'x) "src" 7))
           (decl       (tag (list :declare inner-pair 5) "src" 7)))
      (let ((rebuilt
              (match-pattern decl (:declare (&whole tn type name) val)
                (declare (ignore type name))
                (list :declare tn (* 2 val)))))
        (check rebuilt '(:declare (int x) 10))
        (check (source-loc-start-line (source-loc rebuilt)) 7)
        (check-true (eq (second rebuilt) inner-pair))
        (check (source-loc-start-line (source-loc (second rebuilt))) 7)))))
