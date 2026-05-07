(in-package #:tagless-compiler)
(named-readtables:in-readtable tagless-compiler-syntax)

;;; ---------------------------------------------------------------------------
;;; Test harness
;;; ---------------------------------------------------------------------------

(defvar *tests-run* 0)
(defvar *tests-passed* 0)
(defvar *failures* '())

(defmacro deftest (name &body body)
  `(progn
     (incf *tests-run*)
     (handler-case
         (progn
           ,@body
           (incf *tests-passed*)
           (format t "  PASS ~A~%" ',name))
       (error (e)
         (push (cons ',name e) *failures*)
         (format t "  FAIL ~A: ~A~%" ',name e)))))

(defun reset-tests ()
  (setf *tests-run* 0
        *tests-passed* 0
        *failures* '()))

(defun report-tests ()
  (format t "~%~A/~A passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f (reverse *failures*))
      (format t "  ~A: ~A~%" (car f) (cdr f)))))

;; Assertion helpers. These signal plain ERROR so deftest catches them.
(defun assert-matches (ast rule rules)
  "Pass if AST matches RULE without signaling."
  (multiple-value-bind (success matched remaining)
      (try-match (match-once ast rule rules))
    (declare (ignore matched remaining))
    (unless success
      (error "Expected ~S to match ~S, but it did not." ast rule))))

(defun assert-no-match (ast rule rules)
  "Pass if AST does NOT match RULE."
  (multiple-value-bind (success matched remaining)
      (try-match (match-once ast rule rules))
    (declare (ignore matched remaining))
    (when success
      (error "Expected ~S not to match ~S, but it did." ast rule))))

(defun assert-grammar (ast top-level rules)
  (match-grammar ast top-level rules))

(defun assert-grammar-fails (ast top-level rules)
  (handler-case
      (progn (match-grammar ast top-level rules)
             (error "Expected grammar match against ~S to fail, but it succeeded." top-level))
    (match-error () t)))

;;; ---------------------------------------------------------------------------
;;; Combinator tests (no grammar — empty rules alist)
;;; ---------------------------------------------------------------------------

(defun run-combinator-tests ()
  (let ((rules '()))

    (deftest keyword-matches
      (assert-matches '(:foo) '(keyword :foo) rules))

    (deftest keyword-rejects-wrong
      (assert-no-match '(:bar) '(keyword :foo) rules))

    (deftest keyword-rejects-empty
      (assert-no-match '() '(keyword :foo) rules))

    (deftest identifier-matches-symbol
      (assert-matches '(x) '(identifier) rules))

    (deftest identifier-rejects-keyword
      (assert-no-match '(:foo) '(identifier) rules))

    (deftest identifier-rejects-number
      (assert-no-match '(42) '(identifier) rules))

    (deftest symbol-accepts-keyword
      (assert-matches '(:foo) '(symbol) rules))

    (deftest symbol-accepts-nil
      (assert-matches '(nil) '(symbol) rules))

    (deftest symbol-rejects-number
      (assert-no-match '(42) '(symbol) rules))

    (deftest literal-matches-number
      (assert-matches '(42) '(literal) rules))

    (deftest literal-matches-string
      (assert-matches '("hi") '(literal) rules))

    (deftest literal-matches-bool
      (assert-matches '(t) '(literal) rules))

    (deftest literal-rejects-symbol
      (assert-no-match '(x) '(literal) rules))

    (deftest maybe-matches-when-present
      (multiple-value-bind (success matched remaining)
          (match-once '(42) '(maybe (literal)) rules)
        (unless (and success (eql matched 42) (null remaining))
          (error "maybe should have matched the literal"))))

    (deftest maybe-succeeds-when-absent
      (multiple-value-bind (success matched remaining)
          (match-once '(x) '(maybe (literal)) rules)
        (declare (ignore matched))
        (unless (and success (equal remaining '(x)))
          (error "maybe should have succeeded without consuming"))))

    (deftest option-picks-first
      (multiple-value-bind (success matched remaining)
          (match-once '(42) '(option (literal) (identifier)) rules)
        (declare (ignore remaining))
        (unless (and success (eql matched 42))
          (error "option should pick the literal branch"))))

    (deftest option-falls-through
      (multiple-value-bind (success matched remaining)
          (match-once '(x) '(option (literal) (identifier)) rules)
        (declare (ignore remaining))
        (unless (and success (eq matched 'x))
          (error "option should fall through to identifier"))))

    (deftest option-rejects-no-match
      (assert-no-match '(:foo) '(option (literal) (identifier)) rules))

    (deftest repeat0-empty-input
      (multiple-value-bind (success matched remaining)
          (match-once '() '(repeat0 (literal)) rules)
        (declare (ignore matched))
        (unless (and success (null remaining))
          (error "repeat0 should accept empty input"))))

    (deftest repeat0-consumes-all
      (multiple-value-bind (success matched remaining)
          (match-once '(1 2 3) '(repeat0 (literal)) rules)
        (unless (and success (equal matched '(1 2 3)) (null remaining))
          (error "repeat0 should consume all literals"))))

    (deftest repeat0-stops-on-mismatch
      (multiple-value-bind (success matched remaining)
          (match-once '(1 2 x 3) '(repeat0 (literal)) rules)
        (unless (and success (equal matched '(1 2)) (equal remaining '(x 3)))
          (error "repeat0 should stop at the first mismatch"))))))

;;; ---------------------------------------------------------------------------
;;; Grammar tests against *blub-grammar*
;;; ---------------------------------------------------------------------------

(defun run-grammar-tests ()
  (let ((rules *blub-grammar*))

    (deftest type-int
      (assert-grammar '(:type :int) :type rules))

    (deftest type-pointer-int
      (assert-grammar '(:type (:pointer (:type :int))) :type rules))

    (deftest type-rejects-unknown
      (assert-grammar-fails '(:type :nonsense) :type rules))

    (deftest expr-literal
      (assert-grammar 42 :expr rules))

    (deftest expr-var
      (assert-grammar '(:var x) :expr rules))

    (deftest expr-add
      (assert-grammar '(:add 1 2) :expr rules))

    (deftest expr-nested-add
      (assert-grammar '(:add (:add 1 2) 3) :expr rules))

    (deftest expr-accepts-bare-add
      ;; With :expr dispatched, (:add ...) IS a valid :expr directly —
      ;; no outer (:expr ...) wrapper needed.
      (assert-grammar '(:add 1 2) :expr rules))

    (deftest global-with-init
      (assert-grammar '(:global (:type :double) z 10.2) :global rules))

    (deftest global-without-init
      (assert-grammar '(:global (:type :int) counter) :global rules))

    (deftest function-empty-body
      (assert-grammar
        '(:function (:type :int) main (:args) (:block))
        :function rules))

    (deftest function-with-args-and-body
      (assert-grammar
        '(:function (:type :int) qbe_main
                    (:args (:type :int) x)
                    (:block
                      (:declare (:type :int) y 2)
                      (:assign y (:add (:var x) (:var y)))))
        :function rules))

    (deftest module-mixed
      (assert-grammar
        '(:module
          (:global (:type :double) z 10.2)
          (:global (:type :double) a 1.5)
          (:function (:type :int) main (:args) (:block)))
        :module rules))

    (deftest module-rejects-stray
      (assert-grammar-fails
        '(:module
          (:global (:type :int) x)
          (:nonsense))
        :module rules))

    (deftest module-empty
      (assert-grammar '(:module) :module rules))

    ;; New: control flow and dispatch.

    (deftest if-without-else
      (assert-grammar
        '(:if (:var x)
              (:block (:assign y 1)))
        :if rules))

    (deftest if-with-else
      (assert-grammar
        '(:if (:var x)
              (:block (:assign y 1))
              (:block (:assign y 2)))
        :if rules))

    (deftest while-loop
      (assert-grammar
        '(:while (:lt (:var i) 10)
                 (:block (:assign i (:add (:var i) 1))))
        :while rules))

    (deftest return-with-value
      (assert-grammar '(:return 0) :return rules))

    (deftest return-without-value
      (assert-grammar '(:return) :return rules))

    (deftest break-statement
      (assert-grammar '(:break) :break rules))

    (deftest call-no-args
      (assert-grammar '(:call f) :call rules))

    (deftest call-with-args
      (assert-grammar '(:call printf 1 2 (:var x)) :call rules))

    (deftest comparison-eq
      (assert-grammar '(:eq (:var x) 0) :eq rules))

    (deftest logical-and
      (assert-grammar
        '(:logand (:lt (:var x) 10)
                  (:gt (:var y) 0))
        :logand rules))

    (deftest statement-dispatches-to-if
      (assert-grammar
        '(:block (:if (:var x) (:block) (:block))
                 (:return 0))
        :block rules))

    (deftest function-with-control-flow
      (assert-grammar
        '(:function (:type :int) max
                    (:args (:type :int) a (:type :int) b)
                    (:block
                      (:if (:gt (:var a) (:var b))
                           (:block (:return (:var a)))
                           (:block (:return (:var b))))))
        :function rules))))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------

(defun run-all-tests ()
  (reset-tests)
  (format t "Combinator tests:~%")
  (run-combinator-tests)
  (format t "~%Grammar tests:~%")
  (run-grammar-tests)
  (report-tests))

(run-all-tests)
