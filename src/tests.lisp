;;;; tests.lisp -- Tests for the tagless-compiler interpreter framework.
;;;;
;;;; Assumes the TAGLESS-COMPILER package and its interpreter.lisp have
;;;; already been loaded (e.g. via ASDF). Loading this file runs the suite
;;;; and prints a pass/fail summary. With ASDF, you can wire it up by adding
;;;; a :test-op component or simply :load this file after the system loads.

(in-package #:tagless-compiler)

;;; --- Test harness -----------------------------------------------------

(defvar *results* '())

(defmacro check (label form expected &key (test '#'equal))
  "Evaluate FORM and compare its result to EXPECTED via TEST. Print PASS/FAIL
   and record the outcome in *RESULTS*."
  `(let ((actual ,form))
     (if (funcall ,test actual ,expected)
         (progn (format t "  PASS  ~A~%" ,label)
                (push (list :pass ,label) *results*))
         (progn (format t "  FAIL  ~A~%    expected: ~S~%    actual:   ~S~%"
                        ,label ,expected actual)
                (push (list :fail ,label ,expected actual) *results*)))))

(defmacro check-malformed (label form)
  "FORM should signal MALFORMED-OPERATOR-ARGS."
  `(check ,label
          (handler-case (progn ,form :no-error)
            (malformed-operator-args () :malformed))
          :malformed))

;;; --- Original behavior ------------------------------------------------

(format t "~%=== Original behavior ===~%")

(check "arith-eval on *program*"
       (lower arith-eval *program*)
       18)

(check "string-repr on *program*"
       (lower string-repr *program*)
       "(Mul (Add 1 2) (Inc 5))")

(check "partial-rewrite recurses into unknown heads"
       (lower partial-rewrite '(:if (:square 2) (:square 3) (:add (:square 4) 1)))
       '(:if (:mul 2 2) (:mul 3 3) (:add (:mul 4 4) 1)))

;;; --- Lambda-list features ---------------------------------------------

(format t "~%=== def-op lambda-list features ===~%")

;; &rest support
(defvar rest-interp (make-interpreter))
(def-op rest-interp (:sum &rest xs)
  (reduce #'+ (mapcar #'recurse xs)))

(check ":sum with &rest, 0 args"  (lower rest-interp '(:sum)) 0)
(check ":sum with &rest, 1 arg"   (lower rest-interp '(:sum 5)) 5)
(check ":sum with &rest, 4 args"  (lower rest-interp '(:sum 1 2 3 4)) 10)

;; &key support (with defaults)
(defvar key-interp (make-interpreter))
(def-op key-interp (:scale x &key (by 1) (offset 0))
  (+ (* (recurse x) by) offset))

(check ":scale with defaults"     (lower key-interp '(:scale 10)) 10)
(check ":scale with :by"          (lower key-interp '(:scale 10 :by 3)) 30)
(check ":scale with :by :offset"  (lower key-interp '(:scale 10 :by 3 :offset 5)) 35)

;; &optional support (with defaults)
(defvar opt-interp (make-interpreter))
(def-op opt-interp (:maybe-inc x &optional (n 1))
  (+ (recurse x) n))

(check ":maybe-inc default"  (lower opt-interp '(:maybe-inc 5)) 6)
(check ":maybe-inc explicit" (lower opt-interp '(:maybe-inc 5 10)) 15)

;; Mixed: required + &rest
(defvar mixed-interp (make-interpreter))
(def-op mixed-interp (:cons-up head &rest tail)
  (cons (recurse head) (mapcar #'recurse tail)))

(check ":cons-up mixed"
       (lower mixed-interp '(:cons-up 1 2 3 4))
       '(1 2 3 4))

;;; --- Structured malformed-operator-args condition ---------------------

(format t "~%=== Structured malformed-operator-args condition ===~%")

(handler-case
    (progn (lower arith-eval '(:add 1))
           (format t "  FAIL  malformed :add did not error~%")
           (push '(:fail :no-error) *results*))
  (malformed-operator-args (c)
    (format t "  PASS  malformed :add signals malformed-operator-args~%")
    (push '(:pass :error-type) *results*)
    (check "  operator slot"   (malformed-operator-args-operator c)   :add)
    (check "  expression slot" (malformed-operator-args-expression c) '(:add 1))
    (check "  pattern slot"    (malformed-operator-args-pattern c)    '(:add a b))))

(check "outer handler can recover"
       (handler-case (lower arith-eval '(:add 1))
         (malformed-operator-args (c)
           (declare (ignore c))
           :recovered))
       :recovered)

;; Errors signalled from inside the handler body must NOT be reclassified
;; as malformed-operator-args -- they are real downstream errors.
(defvar body-err-interp (make-interpreter))
(def-op body-err-interp (:divide a b)
  (/ (recurse a) (recurse b)))

(check "body errors propagate as themselves (not malformed-operator-args)"
       (handler-case (lower body-err-interp '(:divide 1 0))
         (malformed-operator-args () :wrongly-caught)
         (division-by-zero ()        :correctly-propagated)
         (arithmetic-error ()        :correctly-propagated))
       :correctly-propagated)

(check-malformed "too few positional args" (lower arith-eval '(:add 1)))
(check-malformed "too many positional args" (lower arith-eval '(:add 1 2 3)))
(check-malformed "odd-length keyword block"
                 (lower key-interp '(:scale 10 :by)))
(check-malformed "unknown keyword in &key block"
                 (lower key-interp '(:scale 10 :nope 5)))
(check-malformed "trailing junk with no &rest"
                 (lower key-interp '(:scale 10 :by 3 99)))

;; Parser-time errors: bad lambda-list shape detected at macroexpansion.
(check "parse error on &rest without var"
       (handler-case
           (progn (macroexpand-1 '(def-op key-interp (:bad &rest))) :no-error)
         (error () :parse-error))
       :parse-error)

(check "parse error on &optional after &key"
       (handler-case
           (progn (macroexpand-1 '(def-op key-interp (:bad &key x &optional y))) :no-error)
         (error () :parse-error))
       :parse-error)

;;; --- CL-package symbols as parameter names ----------------------------

(format t "~%=== CL-package symbols as parameter names ===~%")

;; Parameters whose names happen to be CL symbols (TYPE, COUNT, LIST, ...)
;; must work without tripping package locks.
(defvar cl-name-interp (make-interpreter))

(def-op cl-name-interp (:declare-var name type)
  (format nil "var ~A : ~A" (recurse name) (recurse type)))

(check "cl:type as parameter name"
       (lower cl-name-interp '(:declare-var :x :int))
       "var X : INT")

(def-op cl-name-interp (:counted &optional (count 0))
  (recurse count))

(check "cl:count as &optional name (default)"
       (lower cl-name-interp '(:counted))
       0)
(check "cl:count as &optional name (provided)"
       (lower cl-name-interp '(:counted 7))
       7)

(def-op cl-name-interp (:listing &rest list)
  (length list))

(check "cl:list as &rest name"
       (lower cl-name-interp '(:listing 1 2 3 4))
       4)

;;; --- Cross-interpreter dispatch ---------------------------------------

(format t "~%=== Cross-interpreter dispatch ===~%")

;; A rule defined on one interpreter can dispatch to another by calling the
;; generic LOWER explicitly. RECURSE stays bound to self-recursion.
(defvar fold-interp (make-interpreter :on-unknown :recurse))
(def-op fold-interp (:add a b)
  (let ((aa (recurse a)) (bb (recurse b)))
    (if (and (numberp aa) (numberp bb)) (+ aa bb) (list :add aa bb))))

(defvar print-interp (make-interpreter))
(def-op print-interp (:add a b)
  (format nil "(~A + ~A)" (recurse a) (recurse b)))
(def-op print-interp (:embed-folded x)
  (format nil "[folded: ~A]" (lower fold-interp x)))

(check "self-recurse via RECURSE"
       (lower print-interp '(:add 1 (:add 2 3)))
       "(1 + (2 + 3))")

(check "cross-dispatch via generic LOWER"
       (lower print-interp '(:add 1 (:embed-folded (:add 2 (:add 3 4)))))
       "(1 + [folded: 9])")

;;; --- Summary ----------------------------------------------------------

(format t "~%=== Summary ===~%")
(let ((pass (count :pass *results* :key #'car))
      (fail (count :fail *results* :key #'car)))
  (format t "  ~D passed, ~D failed~%" pass fail)
  (values pass fail))
