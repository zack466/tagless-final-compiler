;;;; Test &whole + match-pattern + match-cases.
;;;;
;;;; This loads inline (mocked) versions of source-loc + the parser
;;;; from v4/parser.lisp, and the macros from v4/util.lisp, so it
;;;; doesn't need fset/eclector loaded.

(defpackage #:m-test (:use #:cl))
(in-package #:m-test)

;; --- Source-loc support ---

(defstruct source-loc
  file start-line start-col end-line end-col start-offset end-offset)

(defvar *source-locations* (make-hash-table :test #'eq))
(defun source-loc (form) (and (consp form) (gethash form *source-locations*)))
(defun (setf source-loc) (loc form)
  (unless (consp form) (error "atom: ~S" form))
  (if loc (setf (gethash form *source-locations*) loc)
          (remhash form *source-locations*)) loc)
(defun clear-source-locations () (clrhash *source-locations*))
(defun inherit-loc (target source)
  (when (consp target)
    (let ((src-loc (source-loc source)))
      (when (and src-loc (not (gethash target *source-locations*)))
        (setf (gethash target *source-locations*) src-loc))))
  target)

;; --- malformed-operator-args ---

(define-condition malformed-operator-args (error)
  ((operator   :initarg :operator   :reader malformed-operator-args-operator)
   (expression :initarg :expression :reader malformed-operator-args-expression)
   (pattern    :initarg :pattern    :initform nil
               :reader malformed-operator-args-pattern))
  (:report (lambda (c stream)
             (format stream "malformed args for ~S: ~S (pattern: ~S)"
                     (malformed-operator-args-operator c)
                     (malformed-operator-args-expression c)
                     (malformed-operator-args-pattern c)))))

;; --- Parser/matcher (with &whole support) ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct parsed-ll whole-var positionals optionals rest-var key-specs)

  (defun parse-pattern (pattern context)
    (cond ((and (symbolp pattern) (not (keywordp pattern))) pattern)
          ((consp pattern) (parse-lambda-list pattern))
          (t (error "bad ~A: ~S" context pattern))))

  (defun parse-lambda-list (lambda-list)
    (unless (listp lambda-list) (error "ll must be list, got ~S" lambda-list))
    (let ((whole-var nil) (positionals '()) (optionals '())
          (rest-var nil) (key-specs '()) (state :positional))
      (when (and lambda-list (eq (first lambda-list) '&whole))
        (pop lambda-list)
        (unless lambda-list (error "&whole needs a var"))
        (let ((v (pop lambda-list)))
          (unless (and (symbolp v) (not (keywordp v)))
            (error "&whole var must be non-kw symbol: ~S" v))
          (setf whole-var v)))
      (labels ((split-spec (spec what)
                 (cond ((symbolp spec) (values spec nil))
                       ((null (cdr spec)) (values (car spec) nil))
                       ((null (cddr spec)) (values (car spec) (cadr spec)))
                       (t (error "bad ~A spec: ~S" what spec))))
               (advance-state (new old-allowed)
                 (unless (member state old-allowed)
                   (error "~S after ~S in ~S" new state lambda-list))
                 (setf state new)))
        (loop while lambda-list do
          (let ((item (pop lambda-list)))
            (case item
              (&whole (error "&whole must be first"))
              (&optional (advance-state :optional '(:positional)))
              (&rest (advance-state :rest '(:positional :optional))
                     (unless lambda-list (error "&rest needs var"))
                     (setf rest-var (parse-pattern (pop lambda-list) "&rest")))
              (&key (advance-state :key '(:positional :optional :rest)))
              (t (ecase state
                   (:positional (push (parse-pattern item "pos") positionals))
                   (:optional
                    (multiple-value-bind (raw d) (split-spec item "&opt")
                      (push (list (parse-pattern raw "&opt") d) optionals)))
                   (:rest (error "extra after &rest: ~S" item))
                   (:key
                    (multiple-value-bind (raw d) (split-spec item "&key")
                      (unless (and (symbolp raw) (not (keywordp raw)))
                        (error "&key var must be sym: ~S" raw))
                      (push (list (intern (symbol-name raw) :keyword) raw d)
                            key-specs)))))))))
      (make-parsed-ll :whole-var whole-var
                      :positionals (nreverse positionals)
                      :optionals (nreverse optionals)
                      :rest-var rest-var
                      :key-specs (nreverse key-specs))))

  (defun flatten-vars (parsed)
    (let ((acc '()))
      (labels ((walk-param (p)
                 (cond ((null p))
                       ((symbolp p) (push p acc))
                       ((parsed-ll-p p) (walk-ll p))))
               (walk-ll (ll)
                 (when (parsed-ll-whole-var ll) (push (parsed-ll-whole-var ll) acc))
                 (dolist (p (parsed-ll-positionals ll)) (walk-param p))
                 (dolist (s (parsed-ll-optionals ll)) (walk-param (first s)))
                 (walk-param (parsed-ll-rest-var ll))
                 (dolist (s (parsed-ll-key-specs ll)) (push (second s) acc))))
        (walk-ll parsed)
        (nreverse acc))))

  (defun emit-parsed-ll (parsed)
    (labels ((emit-param (p)
               (cond ((null p) 'nil)
                     ((symbolp p) `',p)
                     ((parsed-ll-p p) (emit-parsed-ll p))))
             (thunk (form) (if form `(lambda () ,form) 'nil)))
      `(make-parsed-ll
        :whole-var ',(parsed-ll-whole-var parsed)
        :positionals (list ,@(mapcar #'emit-param (parsed-ll-positionals parsed)))
        :optionals (list ,@(loop for (p d) in (parsed-ll-optionals parsed)
                                 collect `(list ,(emit-param p) ,(thunk d))))
        :rest-var ,(emit-param (parsed-ll-rest-var parsed))
        :key-specs (list ,@(loop for (k v d) in (parsed-ll-key-specs parsed)
                                 collect `(list ,k ',v ,(thunk d)))))))
  )

(defun match-lambda-list (parsed args operator expression pattern)
  (flet ((fail ()
           (error 'malformed-operator-args
                  :operator operator :expression expression :pattern pattern)))
    (labels ((match-one (param value)
               (cond ((symbolp param) (list value))
                     ((parsed-ll-p param)
                      (unless (listp value) (fail))
                      (match-ll param value))
                     (t (fail))))
             (match-ll (ll args)
               (let ((values '()) (remaining args))
                 (when (parsed-ll-whole-var ll) (push args values))
                 (dolist (p (parsed-ll-positionals ll))
                   (unless remaining (fail))
                   (dolist (v (match-one p (pop remaining))) (push v values)))
                 (dolist (s (parsed-ll-optionals ll))
                   (let* ((p (first s)) (thunk (second s))
                          (val (if remaining (pop remaining)
                                             (and thunk (funcall thunk)))))
                     (cond ((and (not remaining) (not thunk) (parsed-ll-p p))
                            (dolist (_ (flatten-vars p))
                              (declare (ignore _))
                              (push nil values)))
                           (t (dolist (v (match-one p val)) (push v values))))))
                 (when (parsed-ll-rest-var ll)
                   (let ((rp (parsed-ll-rest-var ll)))
                     (cond ((symbolp rp) (push remaining values))
                           ((parsed-ll-p rp)
                            (dolist (v (match-ll rp remaining))
                              (push v values))))))
                 (cond
                   ((parsed-ll-key-specs ll)
                    (let ((seen (make-hash-table :test #'eq))
                          (allowed (mapcar #'first (parsed-ll-key-specs ll))))
                      (loop while remaining do
                        (let ((k (pop remaining)))
                          (unless (and (keywordp k) (member k allowed)) (fail))
                          (unless remaining (fail))
                          (setf (gethash k seen) (pop remaining))))
                      (dolist (s (parsed-ll-key-specs ll))
                        (let ((kw (first s)) (thunk (third s)))
                          (multiple-value-bind (val present) (gethash kw seen)
                            (push (if present val (and thunk (funcall thunk)))
                                  values))))))
                   ((and (not (parsed-ll-rest-var ll)) remaining) (fail)))
                 (nreverse values))))
      (match-ll parsed args))))

;; --- The two macros under test ---

(defmacro match-pattern (expr (operator &rest lambda-list) &body body)
  (unless (keywordp operator)
    (error "match-pattern head must be keyword: ~S" operator))
  (let* ((parsed (parse-lambda-list lambda-list))
         (all-vars (flatten-vars parsed))
         (g-expr (gensym "EXPR"))
         (g-result (gensym "RESULT")))
    `(let ((,g-expr ,expr))
       (unless (and (consp ,g-expr) (eq (first ,g-expr) ,operator))
         (error 'malformed-operator-args
                :operator ,operator
                :expression ,g-expr
                :pattern '(,operator ,@lambda-list)))
       (let ((,g-result
               (destructuring-bind ,all-vars
                   (match-lambda-list ,(emit-parsed-ll parsed)
                                      (rest ,g-expr)
                                      ,operator ,g-expr
                                      '(,operator ,@lambda-list))
                 ,@body)))
         (inherit-loc ,g-result ,g-expr)))))

(defmacro match-cases (expr &body clauses)
  (let ((g-expr (gensym "EXPR"))
        (g-result (gensym "RESULT"))
        (block-name (gensym "MC")))
    (labels ((emit-clause (clause)
               (cond
                 ((and (consp clause) (eq (car clause) t))
                  `(return-from ,block-name (progn ,@(rest clause))))
                 ((and (consp clause) (consp (car clause)))
                  (destructuring-bind ((operator &rest ll) &rest body) clause
                    (unless (keywordp operator)
                      (error "match-cases head must be keyword: ~S" operator))
                    (let* ((parsed (parse-lambda-list ll))
                           (all-vars (flatten-vars parsed)))
                      `(when (and (consp ,g-expr) (eq (first ,g-expr) ,operator))
                         (return-from ,block-name
                           (destructuring-bind ,all-vars
                               (match-lambda-list ,(emit-parsed-ll parsed)
                                                  (rest ,g-expr)
                                                  ,operator ,g-expr
                                                  '(,operator ,@ll))
                             ,@body))))))
                 (t (error "bad clause: ~S" clause)))))
      `(let* ((,g-expr ,expr)
              (,g-result
                (block ,block-name
                  ,@(mapcar #'emit-clause clauses)
                  (error 'malformed-operator-args
                         :operator (and (consp ,g-expr) (first ,g-expr))
                         :expression ,g-expr
                         :pattern '(match-cases
                                    ,@(loop for c in clauses
                                            when (and (consp c) (consp (car c)))
                                            collect (car c)))))))
         (inherit-loc ,g-result ,g-expr)))))

;; --- Test framework ---

(defvar *tests-run* 0) (defvar *tests-passed* 0) (defvar *failures* '())

(defmacro deftest (name &body body)
  `(progn
     (incf *tests-run*)
     (handler-case (progn ,@body
                          (incf *tests-passed*)
                          (format t "  PASS ~A~%" ',name))
       (error (e)
         (push (cons ',name e) *failures*)
         (format t "  FAIL ~A: ~A~%" ',name e)))))

(defun check (l e a)
  (unless (equalp e a) (error "~A: expected ~S got ~S" l e a)))
(defun check-true (l x) (unless x (error "~A: expected true" l)))
(defun check-false (l x) (when x (error "~A: expected false got ~S" l x)))

(defun tag (form file line)
  (setf (source-loc form)
        (make-source-loc :file file :start-line line :start-col 1
                         :end-line line :end-col 9
                         :start-offset (* line 100)
                         :end-offset (+ (* line 100) 9)))
  form)

;; --- Tests ---

(defun run-tests ()
  (setf *tests-run* 0 *tests-passed* 0 *failures* '())

  (format t "~&=== &whole at top of nested pattern ===~%")

  (deftest whole-binds-original-cons
    (clear-source-locations)
    (let* ((tn (tag (list 'int 'x) "f" 5))
           (input (list :declare tn 0)))
      ;; Match the input: (:declare (&whole pair type name) value)
      (match-pattern input (:declare (&whole pair type name) value)
        (check "type extracted" 'int type)
        (check "name extracted" 'x name)
        (check "value extracted" 0 value)
        (check "pair is original cons" t (eq pair tn))
        (check-true "pair has loc" (source-loc pair))
        (check "pair's loc preserved" 5
               (source-loc-start-line (source-loc pair)))
        ;; Return something to verify match-pattern works
        '(:done))))

  (deftest whole-with-empty-input
    (clear-source-locations)
    ;; Match an operator with no args; &whole on the (rest input) should
    ;; bind to NIL.
    (match-pattern '(:noop) (:noop &whole all)
      (check "all is nil" nil all)
      'ok))

  (format t "~&=== &whole at top level of match-pattern lambda list ===~%")

  (deftest whole-top-level
    ;; (match-pattern expr (:foo &whole all a b)) — `all` binds to
    ;; the args list (rest expr).
    (match-pattern '(:foo 1 2) (:foo &whole all a b)
      (check "all is args" '(1 2) all)
      (check "a" 1 a)
      (check "b" 2 b)
      'ok))

  (format t "~&=== &whole + &optional ===~%")

  (deftest whole-with-optional
    (match-pattern '(:f 1) (:f &whole all a &optional (b 99))
      (check "all" '(1) all)
      (check "a" 1 a)
      (check "b default" 99 b)
      'ok))

  (format t "~&=== &whole + &rest ===~%")

  (deftest whole-with-rest
    (match-pattern '(:f 1 2 3 4) (:f &whole all a &rest tail)
      (check "all" '(1 2 3 4) all)
      (check "a" 1 a)
      (check "tail" '(2 3 4) tail)
      'ok))

  (format t "~&=== Nested &whole keeps original cons through 2 levels ===~%")

  (deftest nested-whole-deep
    (clear-source-locations)
    ;; Build (:f (a (b 1)) end), tag the inner (b 1) and outer (a (b 1)).
    (let* ((inner (tag (list 'b 1) "f" 9))
           (outer (tag (list 'a inner) "f" 8))
           (expr (list :f outer 'end)))
      (match-pattern expr
          (:f (&whole o a (&whole i b val)) tail)
        (check "o is outer" t (eq o outer))
        (check "i is inner" t (eq i inner))
        (check "outer loc" 8 (source-loc-start-line (source-loc o)))
        (check "inner loc" 9 (source-loc-start-line (source-loc i)))
        (check "leaves" '(a b 1) (list a b val))
        (check "tail" 'end tail)
        'ok)))

  (format t "~&=== match-pattern propagates loc to body result ===~%")

  (deftest match-pattern-propagates-loc
    (clear-source-locations)
    (let* ((node (tag (list :double 7) "f" 11)))
      (let ((result (match-pattern node (:double n)
                      (list :pair n n))))
        (check "result built" '(:pair 7 7) result)
        (check-true "result tagged" (source-loc result))
        (check "tagged from node" 11
               (source-loc-start-line (source-loc result))))))

  (deftest match-pattern-doesnt-overwrite
    ;; If body returns a cons that already has a loc, don't overwrite.
    (clear-source-locations)
    (let* ((node (tag (list :double 7) "outer" 11))
           (existing (tag (list :preexisting) "inner" 99)))
      (let ((result (match-pattern node (:double n)
                      (declare (ignore n))
                      existing)))
        (check "returned the existing form" t (eq result existing))
        (check "loc not overwritten" 99
               (source-loc-start-line (source-loc result))))))

  (deftest match-pattern-mismatch-errors
    ;; Wrong head should signal.
    (handler-case
        (progn (match-pattern '(:other) (:declare a) a)
               (error "should have errored"))
      (malformed-operator-args () :ok)))

  (deftest match-pattern-arity-mismatch-errors
    ;; Right head, wrong arity.
    (handler-case
        (progn (match-pattern '(:f 1 2 3) (:f a b) (list a b))
               (error "should have errored"))
      (malformed-operator-args () :ok)))

  (format t "~&=== match-cases: dispatch and propagation ===~%")

  (deftest match-cases-first-clause
    (clear-source-locations)
    (let* ((expr (tag (list :add 1 2) "f" 3)))
      (let ((r (match-cases expr
                 ((:add a b) (list :sum a b))
                 ((:mul a b) (list :prod a b)))))
        (check "first clause" '(:sum 1 2) r)
        (check "loc propagated" 3 (source-loc-start-line (source-loc r))))))

  (deftest match-cases-second-clause
    (let ((r (match-cases '(:mul 3 4)
               ((:add a b) (list :sum a b))
               ((:mul a b) (list :prod a b)))))
      (check "second clause" '(:prod 3 4) r)))

  (deftest match-cases-fallback
    (let ((r (match-cases 'just-a-symbol
               ((:add a b) (list :sum a b))
               (t :catchall))))
      (check "fallback" :catchall r)))

  (deftest match-cases-no-match-errors
    (handler-case
        (progn (match-cases '(:xyz)
                 ((:add a b) (declare (ignore a b)) :sum)
                 ((:mul a b) (declare (ignore a b)) :prod))
               (error "should have errored"))
      (malformed-operator-args () :ok)))

  (deftest match-cases-arity-error-doesnt-fall-through
    ;; (:add) matches the head but has wrong arity. We shouldn't fall
    ;; through to the catch-all -- a wrong-shaped match is a bug we
    ;; want to surface.
    (handler-case
        (progn (match-cases '(:add)
                 ((:add a b) (declare (ignore a b)) :sum)
                 (t :catchall))
               (error "should have errored, not fallen through"))
      (malformed-operator-args () :ok)))

  (deftest match-cases-with-whole
    (clear-source-locations)
    (let* ((tn (tag (list 'int 'y) "f" 12))
           (expr (list :declare tn 5)))
      (let ((r (match-cases expr
                 ((:declare (&whole pair type name) val)
                  (declare (ignore type name val))
                  (list :got pair)))))
        (check "got the cons" t (eq (second r) tn))
        (check "loc still there" 12
               (source-loc-start-line (source-loc (second r)))))))

  (format t "~&=== Errors from parser ===~%")

  (deftest whole-not-first-errors
    (handler-case
        (progn (parse-lambda-list '(a &whole b))
               (error "should have errored"))
      (error () :ok)))

  (deftest whole-needs-var-errors
    (handler-case
        (progn (parse-lambda-list '(&whole))
               (error "should have errored"))
      (error () :ok)))

  (deftest whole-keyword-errors
    (handler-case
        (progn (parse-lambda-list '(&whole :foo a))
               (error "should have errored"))
      (error () :ok)))

  (format t "~&=== match-pattern in a more realistic handler-style use ===~%")

  (deftest realistic-rebuild
    ;; Simulate a handler that destructures a node, rebuilds it, and
    ;; expects the rebuild to inherit the original loc automatically.
    (clear-source-locations)
    (let* ((inner-pair (tag (list 'int 'x) "src" 7))
           (decl (tag (list :declare inner-pair 5) "src" 7)))
      (let ((rebuilt
              (match-pattern decl
                  (:declare (&whole tn type name) val)
                ;; Rebuild with new value: (:declare ORIGINAL-TN (* 2 val))
                ;; tn keeps its loc; the outer (list :declare tn ...)
                ;; cons is a fresh cons that will inherit from `decl`
                ;; via match-pattern's auto-propagation.
                (declare (ignore type name))
                (list :declare tn (* 2 val)))))
        (check "shape" '(:declare (int x) 10) rebuilt)
        (check "outer cons inherits" 7
               (source-loc-start-line (source-loc rebuilt)))
        (check "inner pair is the original cons" t
               (eq (second rebuilt) inner-pair))
        (check "inner pair's loc preserved" 7
               (source-loc-start-line (source-loc (second rebuilt)))))))

  (format t "~&----~%~D/~D tests passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f *failures*) (format t "  ~A: ~A~%" (car f) (cdr f))))
  (= *tests-passed* *tests-run*))

(unless (run-tests) (sb-ext:exit :code 1))
