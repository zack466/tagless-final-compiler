;;;; test-scheme-passes.lisp
;;;;
;;;; Tests for each Scheme compiler pass (1–6b) and the full pipeline.
;;;; Requires the tagless-compiler system to be loaded (via ASDF).

(in-package #:tagless-compiler)

(defsuite "Scheme Pass 1: Uniquify"

  (deftest "scheme-pass-1: shadow rename"
    (let* ((prog '(:module
                   (:define f
                     (:lambda (x)
                       (:let ((x 10))
                         (:var x))))))
           (out (lower *scheme-1* prog))
           (define-form (second out))
           (lam (third define-form))
           (let-form (third lam))
           (inner-x (first (first (second let-form)))))
      ;; Inner x should be renamed because it shadows parameter x.
      (check-true (not (eq inner-x 'x)))))

  (deftest "scheme-pass-1: pre-register globals"
    (let* ((prog '(:module
                   (:define f (:lambda (x) (:apply (:var g) (:var x))))
                   (:define g (:lambda (y) (:var y)))))
           (result (handler-case (progn (lower *scheme-1* prog) :success)
                     (error (e) (format nil "Error: ~A" e)))))
      ;; Because g is pre-registered, f should be able to refer to g without error.
      (check result :success))))


(defsuite "Scheme Pass 2: Assignment conversion"

  (deftest "scheme-pass-2: boxed variable identification"
    (let* ((prog '(:module
                   (:define f
                     (:lambda (x)
                       (:let ((y 10))
                         (:lambda (z)
                           (:set! y 20)
                           (:add (:var y) (:var z))))))))
           (boxable (s2a-vars-to-box prog)))
      ;; y is free in the inner lambda AND mutated by set!, so it must be boxed.
      ;; x is not mutated, z is not captured/free in nested lambda, so only y should be boxed.
      (check-true (fset:contains? boxable 'y))
      (check-false (fset:contains? boxable 'x))
      (check-false (fset:contains? boxable 'z))))

  (deftest "scheme-pass-2: code rewrites for boxed variables"
    (let* ((prog '(:module
                   (:define f
                     (:lambda (x)
                       (:let ((y 10))
                         (:lambda (z)
                           (:set! y 20)
                           (:var y)))))))
           (boxed (s2a-vars-to-box prog))
           (a2b   (let ((*s2b-boxed* boxed))
                    (lower *scheme-2b* prog)))
           (define-form (second a2b))
           (lam (third define-form))
           (let-form (third lam))
           (val (second (first (second let-form))))
           (inner-lam (third let-form))
           (set-stmt (first (cddr inner-lam)))
           (ref-stmt (second (cddr inner-lam))))
      ;; Check that y's binding was converted to :make-ref
      (check (car val) :make-ref)
      ;; Check that (:set! y 20) was converted to (:set-ref! y 20)
      (check (car set-stmt) :set-ref!)
      ;; Check that (:var y) was converted to (:deref y)
      (check (car ref-stmt) :deref))))


(defsuite "Scheme Pass 3: Closure conversion"

  (deftest "scheme-pass-3a: identify free variables"
    (let* ((prog '(:module
                   (:define f
                     (:lambda (x)
                       (:lambda (y)
                         (:add (:var x) (:var y)))))))
           (a3a (lower *scheme-3a* prog))
           (define-form (second a3a))
           (outer-lam (third define-form))
           (inner-lam (fourth outer-lam))
           (free-vars (third inner-lam)))
      ;; The inner lambda should have x as a free variable.
      (check free-vars '(x))))

  (deftest "scheme-pass-3a: globals not captured in closures"
    (let* ((prog '(:module
                   (:define g 42)
                   (:define f
                     (:lambda (x)
                       (:add (:var x) (:var g))))))
           (globals (fset:set 'g))
           (a3a (let ((*scheme-globals* globals))
                  (lower *scheme-3a* prog)))
           (define-form (third a3a)) ; first is :module, second is (:define g 42), third is (:define f ...)
           (lam (third define-form))
           (free-vars (third lam)))
      ;; Global g should not be listed as a free variable in lambda's closure.
      (check-false (member 'g free-vars)))))


(defsuite "Scheme Pass 4: Flatten let"

  (deftest "scheme-pass-4: atomize operands"
    (let* ((prog '(:add (:mul 2 3) 5))
           (out  (lower *scheme-4* prog)))
      ;; (:let ((tmp (:mul 2 3))) (:add (:var tmp) 5))
      (check (car out) :let)
      (let* ((bindings (second out))
             (body (third out))
             (val (second (first bindings))))
        (check (car val) :mul)
        (check (car body) :add)
        (check (car (second body)) :var)))))


(defsuite "Scheme Pass 5: Explicate control"

  (deftest "scheme-pass-5: hoist compound lets out of bindings"
    (let* ((prog '(:let ((x (:let ((z 1)) (:var z)))) (:var x)))
           (out  (lower *scheme-5* prog)))
      ;; Should hoist the nested let out in front of the outer let
      (check (car out) :let)
      (check (car (second (first (second out)))) :let))))


(defsuite "Scheme Pipeline End-to-End"

  (deftest "scheme-pipeline: compile basic program"
    (let* ((prog '(:module
                   (:define main
                     (:lambda ()
                       42))))
           (blub-mod (compile-scheme prog)))
      ;; The pipeline should compile Scheme into a Blub module containing the runtime
      (check (car blub-mod) :module)
      ;; Ensure qbe_main is generated because main is defined
      (check-true (some (lambda (item)
                          (and (consp item)
                               (eq (car item) :function)
                               (eq (third item) 'qbe_main)))
                        blub-mod)))))
