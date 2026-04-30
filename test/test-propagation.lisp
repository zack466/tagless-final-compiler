;;;; End-to-end test of source-loc propagation through interpreter passes.
;;;; Inline-mocks the source-loc system and a stripped-down interpreter
;;;; so we don't need fset/eclector loaded.

(defpackage #:propagation-test (:use #:cl))
(in-package #:propagation-test)

;; --- Source-loc support (stripped from source-locations.lisp) ---

(defstruct source-loc
  file start-line start-col end-line end-col start-offset end-offset)

(defvar *source-locations* (make-hash-table :test #'eq))

(defun source-loc (form)
  (and (consp form) (gethash form *source-locations*)))

(defun (setf source-loc) (loc form)
  (unless (consp form)
    (error "Cannot attach a source location to a non-cons: ~S" form))
  (if loc
      (setf (gethash form *source-locations*) loc)
      (remhash form *source-locations*))
  loc)

(defun clear-source-locations () (clrhash *source-locations*))

(defun inherit-loc (target source)
  (when (consp target)
    (let ((src-loc (source-loc source)))
      (when (and src-loc (not (gethash target *source-locations*)))
        (setf (gethash target *source-locations*) src-loc))))
  target)

(defun some-recorded-offset (form)
  (cond ((not (consp form)) nil)
        ((source-loc form) (source-loc-start-offset (source-loc form)))
        (t (or (some-recorded-offset (car form))
               (some-recorded-offset (cdr form))))))

(defun smallest-containing-range (offset)
  (let ((best nil) (best-size most-positive-fixnum))
    (maphash (lambda (cons loc)
               (declare (ignore cons))
               (let ((s (source-loc-start-offset loc))
                     (e (source-loc-end-offset loc)))
                 (when (and s e (<= s offset) (<= offset e))
                   (let ((size (- e s)))
                     (when (< size best-size)
                       (setf best loc best-size size))))))
             *source-locations*)
    best))

(defun source-loc-or-ancestor (form)
  (or (source-loc form)
      (let ((self-offset (some-recorded-offset form)))
        (when self-offset (smallest-containing-range self-offset)))))

(defun format-source-loc (loc &key (stream nil))
  (when loc
    (format stream "~A:~D:~D"
            (or (source-loc-file loc) "<unknown>")
            (source-loc-start-line loc)
            (source-loc-start-col loc))))

;; --- Stripped-down interpreter (just the propagation-relevant bits) ---

(defstruct splice (nodes nil :type list))
(defun splice (&rest nodes) (make-splice :nodes nodes))

(defclass interpreter ()
  ((handlers :initform (make-hash-table :test #'eq) :reader handlers)
   (on-unknown :initarg :on-unknown :initform :error :accessor on-unknown)
   (propagate-source-locations :initarg :propagate-source-locations
                               :initform t
                               :accessor propagate-source-locations)))

(defun make-interpreter (&key (on-unknown :error)
                              (propagate-source-locations t))
  (make-instance 'interpreter
                 :on-unknown on-unknown
                 :propagate-source-locations propagate-source-locations))

(defgeneric lower (interp expr &key splice))

(defun coerce-result (result splice-context-p op expr)
  (declare (ignore op expr))
  (cond
    (splice-context-p
     (if (splice-p result) (splice-nodes result) (list result)))
    ((splice-p result)
     (error "splice in expression context"))
    (t result)))

(defun propagate-locs (coerced source splice-context-p)
  (cond (splice-context-p
         (dolist (node coerced) (inherit-loc node source)))
        (t (inherit-loc coerced source))))

(defmethod lower ((interp interpreter) (expr cons) &key splice)
  (let* ((op (first expr))
         (handler (gethash op (handlers interp)))
         (result (cond
                   (handler (funcall handler expr))
                   ((eq (on-unknown interp) :passthrough) expr)
                   ((eq (on-unknown interp) :recurse)
                    (inherit-loc
                     (cons (first expr)
                           (mapcar (lambda (a) (lower interp a)) (rest expr)))
                     expr))
                   (t (error "unknown op ~S" op))))
         (coerced (coerce-result result splice op expr)))
    (when (propagate-source-locations interp)
      (propagate-locs coerced expr splice))
    coerced))

(defmethod lower ((interp interpreter) (expr t) &key splice)
  (coerce-result expr splice nil expr))

;; --- Test framework ---

(defvar *tests-run* 0)
(defvar *tests-passed* 0)
(defvar *failures* '())

(defmacro deftest (name &body body)
  `(progn
     (incf *tests-run*)
     (handler-case
         (progn ,@body
                (incf *tests-passed*)
                (format t "  PASS ~A~%" ',name))
       (error (e)
         (push (cons ',name e) *failures*)
         (format t "  FAIL ~A: ~A~%" ',name e)))))

(defun check (label expected actual)
  (unless (equalp expected actual)
    (error "~A: expected ~S, got ~S" label expected actual)))

(defun check-true (label x) (unless x (error "~A: expected true" label)))
(defun check-false (label x) (when x (error "~A: expected false, got ~S" label x)))

;; --- Helper: tag a freshly-built form with a synthetic loc ---

(defun tag (form file line)
  (setf (source-loc form)
        (make-source-loc :file file :start-line line :start-col 1
                         :end-line line :end-col 10
                         :start-offset (* line 100)
                         :end-offset (+ (* line 100) 10)))
  form)

;; --- Tests ---

(defun run-tests ()
  (setf *tests-run* 0 *tests-passed* 0 *failures* '())

  (format t "~&=== Auto-propagation: simple cases ===~%")

  (deftest passthrough-preserves-loc
    ;; on-unknown :passthrough returns the input verbatim, so loc stays.
    (clear-source-locations)
    (let* ((interp (make-interpreter :on-unknown :passthrough))
           (input (tag (list :unknown 1 2) "f.lisp" 5)))
      (let ((result (lower interp input)))
        (check "result eq input" t (eq result input))
        (check-true "loc preserved" (source-loc result)))))

  (deftest fresh-cons-from-handler-inherits
    ;; A handler that returns a fresh cons should have it tagged
    ;; automatically by lower's auto-propagation.
    (clear-source-locations)
    (let* ((interp (make-interpreter)))
      (setf (gethash :double (handlers interp))
            (lambda (expr)
              (declare (ignore expr))
              (list :pair :hello :world)))
      (let* ((input (tag (list :double) "f.lisp" 7))
             (result (lower interp input)))
        (check-true "result is a fresh list" (consp result))
        (check-false "result is not the input" (eq result input))
        (let ((loc (source-loc result)))
          (check-true "fresh result tagged" loc)
          (check "tagged with input's loc" 7 (source-loc-start-line loc))))))

  (deftest atom-result-no-error
    ;; Handler returning an atom shouldn't cause inherit-loc to error.
    (clear-source-locations)
    (let ((interp (make-interpreter)))
      (setf (gethash :extract-atom (handlers interp))
            (lambda (expr) (declare (ignore expr)) 42))
      (let* ((input (tag (list :extract-atom) "f.lisp" 3))
             (result (lower interp input)))
        (check "atom returned" 42 result))))

  (format t "~&=== Auto-propagation: splice context ===~%")

  (deftest splice-each-node-tagged
    ;; A splice handler returns multiple nodes; each should inherit
    ;; from the input.
    (clear-source-locations)
    (let ((interp (make-interpreter :on-unknown :passthrough)))
      (setf (gethash :triple (handlers interp))
            (lambda (expr)
              (declare (ignore expr))
              (splice (list :a) (list :b) (list :c))))
      ;; To exercise splice context, wrap in a :block-style handler.
      (setf (gethash :block (handlers interp))
            (lambda (expr)
              (cons :block (mapcan (lambda (x) (lower interp x :splice t))
                                   (rest expr)))))
      (let* ((triple (tag (list :triple) "f.lisp" 4))
             (block-form (tag (list :block triple) "f.lisp" 4))
             (result (lower interp block-form)))
        ;; Block result: (:block (:a) (:b) (:c))
        (check "block result shape" '(:block (:a) (:b) (:c)) result)
        ;; Each spliced sub-list should be tagged with the :triple's loc
        (let ((loc-a (source-loc (second result)))
              (loc-b (source-loc (third result)))
              (loc-c (source-loc (fourth result))))
          (check-true "sub-a tagged" loc-a)
          (check-true "sub-b tagged" loc-b)
          (check-true "sub-c tagged" loc-c)
          (check "sub-a from triple loc" 4 (source-loc-start-line loc-a))))))

  (format t "~&=== First-wins: handler-set locs preserved ===~%")

  (deftest explicit-handler-loc-wins
    ;; If a handler explicitly tags a node (e.g. inheriting from a
    ;; sub-expression rather than the whole input), auto-propagation
    ;; mustn't overwrite it.
    (clear-source-locations)
    (let ((interp (make-interpreter)))
      ;; Handler that picks the first arg's loc to attribute its output.
      (setf (gethash :sub-from (handlers interp))
            (lambda (expr)
              (let ((sub  (second expr))
                    (out  (list :result :v)))
                (inherit-loc out sub)
                out)))
      (let* ((sub-form (tag (list :inner) "inner.lisp" 99))
             (input (tag (list :sub-from sub-form) "outer.lisp" 5))
             (result (lower interp input)))
        (let ((loc (source-loc result)))
          (check-true "result tagged" loc)
          (check "from sub, not parent" 99 (source-loc-start-line loc))
          (check "sub's file" "inner.lisp" (source-loc-file loc))))))

  (format t "~&=== Recurse-on-unknown rebuilds and inherits ===~%")

  (deftest recurse-rebuilds-with-loc
    ;; :on-unknown :recurse rebuilds (head args...) with each arg
    ;; recursively lowered. The rebuilt cons should inherit the input's
    ;; loc.
    (clear-source-locations)
    (let ((interp (make-interpreter :on-unknown :recurse)))
      ;; Define :double; leave :wrap unknown so it triggers recurse.
      (setf (gethash :double (handlers interp))
            (lambda (expr)
              (let ((x (second expr)))
                (list :pair x x))))
      (let* ((inner (tag (list :double 7) "f.lisp" 10))
             (outer (tag (list :wrap inner) "f.lisp" 11))
             (result (lower interp outer)))
        (check "result shape" '(:wrap (:pair 7 7)) result)
        (let ((outer-loc (source-loc result))
              (inner-loc (source-loc (second result))))
          (check-true "outer rebuilt cons tagged" outer-loc)
          (check "outer from line 11" 11 (source-loc-start-line outer-loc))
          (check-true "inner result tagged" inner-loc)
          (check "inner from line 10" 10 (source-loc-start-line inner-loc))))))

  (format t "~&=== Propagation across multiple passes ===~%")

  (deftest end-to-end-multi-pass
    ;; Build an AST with locs, run pass 1 (which produces fresh nodes),
    ;; then pass 2 (which builds further fresh nodes from pass-1 output).
    ;; After both passes, leaf nodes should still trace back to the
    ;; original source line.
    (clear-source-locations)
    (let ((pass1 (make-interpreter :on-unknown :recurse))
          (pass2 (make-interpreter :on-unknown :recurse)))
      ;; Pass 1: :square -> (:mul x x)
      (setf (gethash :square (handlers pass1))
            (lambda (expr)
              (let ((x (second expr)))
                (list :mul x x))))
      ;; Pass 2: :mul -> (:builtin-multiply a b)
      (setf (gethash :mul (handlers pass2))
            (lambda (expr)
              (list :builtin-multiply (second expr) (third expr))))
      (let* ((sq (tag (list :square 5) "src.lisp" 42))
             (after-1 (lower pass1 sq))
             (after-2 (lower pass2 after-1)))
        (check "shape after both" '(:builtin-multiply 5 5) after-2)
        ;; The final node was built fresh by pass 2 from pass-1's output.
        ;; Pass 2's auto-propagation tags it with pass-1's output loc,
        ;; which (via pass-1's auto-propagation) was tagged from the
        ;; original :square at line 42.
        (let ((loc (source-loc after-2)))
          (check-true "final node tagged" loc)
          (check "still points at line 42" 42 (source-loc-start-line loc))
          (check "still points at src.lisp" "src.lisp" (source-loc-file loc))))))

  (format t "~&=== Disabling propagation works ===~%")

  (deftest propagation-disabled
    (clear-source-locations)
    (let ((interp (make-interpreter :propagate-source-locations nil)))
      (setf (gethash :ident (handlers interp))
            (lambda (expr) (list :result (second expr))))
      (let* ((input (tag (list :ident 'x) "f.lisp" 1))
             (result (lower interp input)))
        (check-false "no auto-prop" (source-loc result)))))

  (format t "~&=== Splice with empty result is fine ===~%")

  (deftest empty-splice
    (clear-source-locations)
    (let ((interp (make-interpreter :on-unknown :passthrough)))
      (setf (gethash :drop (handlers interp))
            (lambda (expr) (declare (ignore expr)) (splice)))
      (setf (gethash :block (handlers interp))
            (lambda (expr)
              (cons :block (mapcan (lambda (x) (lower interp x :splice t))
                                   (rest expr)))))
      (let* ((drop (tag (list :drop) "f.lisp" 1))
             (block-form (tag (list :block drop) "f.lisp" 1))
             (result (lower interp block-form)))
        (check "block empty after drop" '(:block) result))))

  (format t "~&----~%~D/~D tests passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f *failures*) (format t "  ~A: ~A~%" (car f) (cdr f))))
  (= *tests-passed* *tests-run*))

(unless (run-tests) (sb-ext:exit :code 1))
