;;;; test/harness.lisp — unified test harness for tagless-compiler tests.
;;;;
;;;; All test files in #:tagless-compiler load this first and use its API.
;;;;
;;;; API:
;;;;   *VERBOSE*          — when T, print passing tests (default: NIL, quiet on pass)
;;;;   DEFTEST label body — atomic test; silent on pass, prints FAIL with message
;;;;   CHECK actual expected &key test — errors with expected/got on mismatch
;;;;   CHECK-TRUE actual  — errors if ACTUAL is NIL
;;;;   CHECK-FALSE actual — errors if ACTUAL is non-NIL
;;;;   CHECK-ERROR form [condition-type] — errors if FORM does not signal
;;;;   DEFSUITE name body — named group; prints one-line summary
;;;;
;;;; IMPORTANT: DEFSUITE expands its body with dynamically rebound counters.
;;;; Any DEFVAR or DEF-OP setup must appear OUTSIDE DEFSUITE, not inside it.

(in-package #:tagless-compiler)

(defvar *verbose* nil
  "When T, also print passing tests. Default NIL: only failures are shown.")

;;; Per-suite pass/fail counters. Dynamically rebound by DEFSUITE.
(defvar *suite-pass* 0)
(defvar *suite-fail* 0)

(defmacro deftest (label &body body)
  "Run BODY as an atomic test named LABEL.
On success: increments *SUITE-PASS*; prints only when *VERBOSE*.
On failure: increments *SUITE-FAIL*; always prints the condition."
  (let ((err (gensym "ERR")))
    `(handler-case
         (progn ,@body
                (incf *suite-pass*)
                (when *verbose* (format t "  pass  ~A~%" ,label)))
       (error (,err)
         (incf *suite-fail*)
         (format t "  FAIL  ~A~%        ~A~%" ,label ,err)))))

(defun check (actual expected &key (test #'equal))
  "Assert (FUNCALL TEST ACTUAL EXPECTED) is true.
Signals an error showing expected and actual values on mismatch."
  (unless (funcall test actual expected)
    (error "~%  expected: ~S~%  but got:  ~S" expected actual)))

(defun check-true (actual)
  "Assert ACTUAL is truthy. Signals an error if it is NIL or false."
  (unless actual
    (error "~%  expected truthy but got: ~S" actual)))

(defun check-false (actual)
  "Assert ACTUAL is NIL. Signals an error if it is non-NIL."
  (when actual
    (error "~%  expected NIL but got: ~S" actual)))

(defmacro check-error (form &optional (condition-type 'error))
  "Assert FORM signals a condition of type CONDITION-TYPE (default ERROR).
Signals an error if FORM returns normally without signaling."
  (let ((signaled-p (gensym "SIG")))
    `(let ((,signaled-p nil))
       (handler-case (progn ,form)
         (,condition-type () (setf ,signaled-p t)))
       (unless ,signaled-p
         (error "~%  expected ~S to signal ~S, but it returned normally"
                ',form ',condition-type)))))

(defmacro defsuite (name &body tests)
  "Run TESTS grouped under NAME, with fresh pass/fail counters.
Prints a one-line summary: \"NAME: N/TOTAL passed\" or \"NAME: N/TOTAL passed, M FAILED\".

IMPORTANT: DEFVAR, DEF-OP, and other setup forms must appear OUTSIDE this
macro, not inside it, because the body runs inside a LET rebinding special
variables."
  `(let ((*suite-pass* 0) (*suite-fail* 0))
     ,@tests
     (let ((total (+ *suite-pass* *suite-fail*)))
       (if (zerop *suite-fail*)
           (format t "  ~A: ~D/~D passed~%" ,name *suite-pass* total)
           (format t "  ~A: ~D/~D passed, ~D FAILED~%" ,name *suite-pass* total *suite-fail*)))))
