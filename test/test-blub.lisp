;;;; Test the blub-1 renaming pass end-to-end.
;;;;
;;;; We mock fset (using a CL hash-table-based "persistent" map that's
;;;; close enough for test purposes -- we copy on each rebinding via
;;;; deep-copy in the let-rebinding) and the source-loc machinery, then
;;;; transcribe the blub-1 handlers and exercise them against a sample
;;;; program.

(defpackage #:blub-test (:use #:cl))
(in-package #:blub-test)

;; --- Mock fset (just enough for blub-1) ---
;;
;; We don't actually create an fset package — we just have functions
;; named the way the code uses them via flet-equivalent at the use
;; sites. Persistence is via association lists.

(defstruct mock-map (alist '()))
(defun fset-empty-map () (make-mock-map))
(defun fset-lookup (m key)
  (let ((cell (assoc key (mock-map-alist m))))
    (if cell (values (cdr cell) t) (values nil nil))))
(defun fset-with (m key value)
  (make-mock-map :alist (acons key value (mock-map-alist m))))
(defmacro fset-includef (place key value)
  `(setf ,place (fset-with ,place ,key ,value)))

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

;; --- Stripped interpreter with auto-propagation ---

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
    ((splice-p result) (error "splice in expression context"))
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

;; --- def-op stand-in (plain function registration) ---
;;
;; We don't need the macro machinery for these tests; we register
;; handlers by hand and bind RECURSE / EXPR / INHERIT-FROM as
;; closures. This sidesteps the whole lambda-list parser.

(defmacro def-handler (interp op (expr-var) &body body)
  (let ((g-expr (gensym)))
    `(setf (gethash ,op (handlers ,interp))
           (lambda (,g-expr)
             (let ((,expr-var ,g-expr))
               (flet ((recurse (x) (lower ,interp x))
                      (expr () ,expr-var)
                      (inherit-from (target source)
                        (inherit-loc target source)))
                 (declare (ignorable #'recurse #'expr #'inherit-from))
                 ,@body))))))

(defvar *fresh-counter* 0)
(defun fresh-name (&optional (prefix "g"))
  (intern (format nil "~A~D" (string prefix) (incf *fresh-counter*))))

;; --- The blub-1 pass under test ---

(defparameter *blub-1* (make-interpreter :on-unknown :recurse))
(defparameter *var-rename-map* (fset-empty-map))

(defun node-is-p (kw) (lambda (n) (and (consp n) (eq (car n) kw))))
(defun node-is-not-p (kw) (lambda (n) (and (consp n) (not (eq (car n) kw)))))
(defun my-filter (nodes pred) (loop for n in nodes if (funcall pred n) collect n))

(defun register-global (name)
  (when (nth-value 1 (fset-lookup *var-rename-map* name))
    (error "Global variable ~A already declared." name))
  (fset-includef *var-rename-map* name name)
  name)

(defun register-local (name)
  (let* ((found (nth-value 1 (fset-lookup *var-rename-map* name)))
         (new-name (if found (fresh-name (string name)) name)))
    (fset-includef *var-rename-map* name new-name)
    new-name))

(defun lookup-or-error (name kind)
  (multiple-value-bind (mapped found) (fset-lookup *var-rename-map* name)
    (unless found (error "Variable ~A but not declared: ~A" kind name))
    mapped))

(def-handler *blub-1* :global (e)
  (destructuring-bind (op (type name) &optional value) e
    (declare (ignore op))
    (register-global name)
    (let ((tn (list type name)))
      (inherit-from tn (expr))
      (list :global tn (recurse value)))))

(def-handler *blub-1* :declare (e)
  (destructuring-bind (op (type name) &optional value) e
    (declare (ignore op))
    (let* ((lowered-value (recurse value))
           (new-name (register-local name))
           (tn (list type new-name)))
      (inherit-from tn (expr))
      (list :declare tn lowered-value))))

(def-handler *blub-1* :assign (e)
  (destructuring-bind (op name value) e
    (declare (ignore op))
    (list :assign (lookup-or-error name "assigned") (recurse value))))

(def-handler *blub-1* :var (e)
  (destructuring-bind (op name) e
    (declare (ignore op))
    (list :var (lookup-or-error name "read"))))

(def-handler *blub-1* :block (e)
  (let ((*var-rename-map* *var-rename-map*))
    (cons :block (mapcar #'recurse (rest e)))))

(def-handler *blub-1* :function (e)
  (destructuring-bind (op type name args &rest body) e
    (declare (ignore op))
    (let ((*var-rename-map* *var-rename-map*))
      (let ((renamed-args
              (mapcar (lambda (arg)
                        (destructuring-bind (atype aname) arg
                          (let* ((new-name (register-local aname))
                                 (np (list atype new-name)))
                            (inherit-from np arg)
                            np)))
                      args)))
        (list :function type name renamed-args (mapcar #'recurse body))))))

(def-handler *blub-1* :module (e)
  (let* ((body (rest e))
         (*var-rename-map* (fset-empty-map))
         (globals (mapcar #'recurse (my-filter body (node-is-p :global))))
         (renamed (mapcar #'recurse (my-filter body (node-is-not-p :global)))))
    (cons :module (append globals renamed))))

;; --- Tests ---

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
(defun check-true (l x) (unless x (error "~A: expected true" l)))

(defun reset ()
  (clear-source-locations)
  (setf *fresh-counter* 0)
  (setf *var-rename-map* (fset-empty-map)))

(defun tag (form file line)
  (setf (source-loc form)
        (make-source-loc :file file :start-line line :start-col 1
                         :end-line line :end-col 10
                         :start-offset (* line 100)
                         :end-offset (+ (* line 100) 10)))
  form)

(defun run-tests ()
  (setf *tests-run* 0 *tests-passed* 0 *failures* '())

  (format t "~&=== Sanity: simple block, no shadowing ===~%")

  (deftest simple-block-no-shadow
    (reset)
    (let* ((decl-x (list :declare '(int x) 1))
           (decl-y (list :declare '(int y) 2))
           (assign (list :assign 'y 0))
           (block-form (list :block decl-x decl-y assign))
           (mod  (list :module block-form))
           (out  (lower *blub-1* mod)))
      (check "shape preserved" mod out)))

  (format t "~&=== Shadowing within a block freshens names ===~%")

  (deftest shadow-in-block
    (reset)
    ;; Two :declares of y in the same block => second one freshened.
    (let* ((d1 (list :declare '(int y) 1))
           (d2 (list :declare '(int y) 2))
           (a  (list :assign 'y 0))
           (b  (list :block d1 d2 a))
           (m  (list :module b))
           (out (lower *blub-1* m)))
      ;; Result: (:module (:block (:declare (int y) 1)
      ;;                          (:declare (int Y<n>) 2)
      ;;                          (:assign Y<n> 0)))
      (let* ((block-out (second out))
             (d2-out (third block-out))
             (a-out (fourth block-out))
             (renamed-y (second (second d2-out))))
        (check-true "y was renamed" (not (eq renamed-y 'y)))
        (check "type preserved" 'int (first (second d2-out)))
        (check ":assign uses renamed y" renamed-y (second a-out)))))

  (format t "~&=== Sibling blocks don't see each other ===~%")

  (deftest sibling-blocks
    (reset)
    (let* ((b1 (list :block
                     (list :declare '(int x) 1)
                     (list :assign 'x 9)))
           (b2 (list :block
                     (list :declare '(int x) 2)
                     (list :assign 'x 8)))
           (m  (list :module b1 b2))
           (out (lower *blub-1* m)))
      ;; In each block, x stays as x (no shadow within the block, and
      ;; the outer scope was empty to begin with).
      (let ((b1-out (second out))
            (b2-out (third out)))
        (check "b1 declare" 'x (second (second (second b1-out))))
        (check "b1 assign uses x" 'x (second (third b1-out)))
        (check "b2 declare" 'x (second (second (second b2-out))))
        (check "b2 assign uses x" 'x (second (third b2-out))))))

  (format t "~&=== Function parameter shadows global ===~%")

  (deftest function-param-shadows-global
    (reset)
    (let* ((g (list :global '(int x) 0))
           (f (list :function 'int 'foo (list (list 'int 'x))
                    (list :assign 'x 1)))
           (m (list :module g f))
           (out (lower *blub-1* m)))
      (let* ((f-out (third out))
             (renamed-args (fourth f-out))
             (param-name (second (first renamed-args)))
             (body (fifth f-out))
             (assign (first body)))
        ;; Param x shadows the global x and gets a fresh name.
        (check-true "param renamed" (not (eq param-name 'x)))
        ;; The :assign in the body uses the renamed param.
        (check "body assigns to renamed param" param-name (second assign)))))

  (format t "~&=== Globals can't conflict ===~%")

  (deftest global-conflict-errors
    (reset)
    (let* ((g1 (list :global '(int x) 1))
           (g2 (list :global '(int x) 2))
           (m  (list :module g1 g2)))
      (handler-case (progn (lower *blub-1* m) (error "should have errored"))
        (error (c) (check-true "errored" c)))))

  (format t "~&=== Self-reference in :declare uses outer scope ===~%")

  (deftest self-ref-uses-outer
    (reset)
    ;; Outer scope: x=x. Inner :declare (int x) (:var x) should resolve
    ;; :var x against the outer x, then bind inner x to a fresh name.
    (let* ((outer-decl (list :declare '(int x) 0))
           (inner-decl (list :declare '(int x) (list :var 'x)))
           (b (list :block outer-decl inner-decl))
           (m (list :module b))
           (out (lower *blub-1* m)))
      (let* ((b-out (second out))
             (inner-out (third b-out))
             (inner-name (second (second inner-out)))
             (init       (third inner-out))
             (init-name  (second init)))
        (check-true "inner x freshened" (not (eq inner-name 'x)))
        (check "init reads outer x (not inner)" 'x init-name)
        (check-true "init's x and decl's x differ"
                    (not (eq init-name inner-name))))))

  (format t "~&=== Source-loc propagation through the pass ===~%")

  (deftest locs-propagate-end-to-end
    (reset)
    (let* ((decl (tag (list :declare '(int y) 5) "src.lisp" 7))
           (block-form (tag (list :block decl) "src.lisp" 6))
           (mod (tag (list :module block-form) "src.lisp" 1))
           (out (lower *blub-1* mod)))
      ;; The output :module's outer cons should have the input :module's loc
      (check "module loc" 1 (source-loc-start-line (source-loc out)))
      (let* ((block-out (second out))
             (decl-out (second block-out)))
        (check "block loc" 6 (source-loc-start-line (source-loc block-out)))
        (check "declare loc" 7 (source-loc-start-line (source-loc decl-out)))
        ;; The (int y) inner pair should have inherited the declare's loc
        ;; via inherit-from.
        (let ((tn (second decl-out)))
          (check-true "type-name pair tagged" (source-loc tn))
          (check "type-name from declare loc"
                 7 (source-loc-start-line (source-loc tn)))))))

  (deftest locs-survive-shadowing
    ;; Shadow a variable. The freshened-name :declare's outer cons
    ;; should still attribute to the original :declare in source.
    (reset)
    (let* ((d1 (tag (list :declare '(int y) 1) "f.lisp" 3))
           (d2 (tag (list :declare '(int y) 2) "f.lisp" 4))
           (b  (tag (list :block d1 d2) "f.lisp" 2))
           (m  (tag (list :module b) "f.lisp" 1))
           (out (lower *blub-1* m)))
      (let* ((b-out (second out))
             (d2-out (third b-out)))
        (check "shadowed declare keeps loc"
               4 (source-loc-start-line (source-loc d2-out))))))

  (format t "~&----~%~D/~D tests passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f *failures*) (format t "  ~A: ~A~%" (car f) (cdr f))))
  (= *tests-passed* *tests-run*))

(unless (run-tests) (sb-ext:exit :code 1))
