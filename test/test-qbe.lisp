;;;; Tests for the QBE validation logic.
;;;;
;;;; We can't load the actual interpreter (needs fset), so we inline
;;;; the validation helpers and a hand-rolled version of the key
;;;; validation handlers. The logic under test is identical to what
;;;; def-op generates -- just expressed without the macro.

(defpackage #:qbe-test (:use #:cl))
(in-package #:qbe-test)

;; --- Source-loc support (minimal) ---

(defstruct source-loc
  file start-line start-col end-line end-col start-offset end-offset)

(defvar *source-locations* (make-hash-table :test #'eq))
(defun source-loc (form) (and (consp form) (gethash form *source-locations*)))
(defun (setf source-loc) (loc form)
  (unless (consp form) (error "atom: ~S" form))
  (if loc (setf (gethash form *source-locations*) loc)
          (remhash form *source-locations*)) loc)

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

(defun clear-locs () (clrhash *source-locations*))
(defun tag (form file line)
  (setf (source-loc form)
        (make-source-loc :file file :start-line line :start-col 1
                         :end-line line :end-col 9
                         :start-offset (* line 100)
                         :end-offset (+ (* line 100) 9)))
  form)

;; --- Validation condition + helpers (verbatim copy from qbe.lisp) ---

(define-condition qbe-validation-error (error)
  ((message    :initarg :message    :reader qbe-validation-error-message)
   (expression :initarg :expression :reader qbe-validation-error-expression))
  (:report
   (lambda (c stream)
     (format stream "~A~@[~%  in: ~S~]~@[~%  at: ~A~]"
             (qbe-validation-error-message c)
             (qbe-validation-error-expression c)
             (let ((loc (source-loc-or-ancestor
                         (qbe-validation-error-expression c))))
               (when loc (format-source-loc loc)))))))

(defun qbe-error (expression fmt &rest args)
  (error 'qbe-validation-error
         :message (apply #'format nil fmt args)
         :expression expression))

(defparameter *qbe-base-types* '(:w :l :s :d))
(defparameter *qbe-subw-types* '(:sb :ub :sh :uh))
(defparameter *qbe-ext-types* '(:w :l :s :d :b :h :z))
(defparameter *qbe-special-types* '(:... :env))
(defparameter *qbe-assign-opcodes*
  '(:add :sub :div :mul :neg :udiv :rem :urem :or :xor :and
    :sar :shr :shl :loadw :copy))   ; subset is enough for tests
(defparameter *qbe-effect-opcodes*
  '(:storew :storel :stores :stored :storeb :storeh :blit :vastart))

(defun qbe-aggregate-type-p (x)
  (or (stringp x)
      (and (consp x) (eq (first x) :user-type))
      (and (keywordp x)
           (not (member x *qbe-base-types* :test #'eq))
           (not (member x *qbe-subw-types* :test #'eq))
           (not (member x *qbe-ext-types* :test #'eq))
           (not (member x *qbe-special-types* :test #'eq)))))

(defun qbe-abity-p (x)
  (or (member x *qbe-base-types* :test #'eq)
      (member x *qbe-subw-types* :test #'eq)
      (qbe-aggregate-type-p x)))

(defun check-base-type (type expression context)
  (unless (member type *qbe-base-types* :test #'eq)
    (qbe-error expression "Invalid base type ~S in ~A. Expected one of ~S."
               type context *qbe-base-types*)))
(defun check-ext-type (type expression context)
  (unless (member type *qbe-ext-types* :test #'eq)
    (qbe-error expression "Invalid extended type ~S in ~A. Expected one of ~S."
               type context *qbe-ext-types*)))
(defun check-abity (type expression context)
  (unless (qbe-abity-p type)
    (qbe-error expression "Invalid ABITY ~S in ~A." type context)))
(defun check-assign-opcode (op expression)
  (unless (member op *qbe-assign-opcodes* :test #'eq)
    (qbe-error expression "Invalid assignment opcode ~S." op)))
(defun check-effect-opcode (op expression)
  (unless (member op *qbe-effect-opcodes* :test #'eq)
    (qbe-error expression "Invalid effectful opcode ~S." op)))
(defun check-cons-with-head (form head expression context)
  (unless (and (consp form) (eq (first form) head))
    (qbe-error expression "~A: expected a (~S ...) form, got ~S."
               context head form)))

;; --- Linkage formatting ---

(defun format-linkage-flag (flag)
  (cond ((stringp flag) flag)
        ((or (keywordp flag) (symbolp flag)) (format nil "~(~a~)" flag))
        (t (format nil "~a" flag))))

(defun format-linkage (linkage-list)
  (cond ((null linkage-list) "")
        ((atom linkage-list)
         (format nil "~a " (format-linkage-flag linkage-list)))
        (t (format nil "~{~a~^ ~} "
                   (mapcar #'format-linkage-flag linkage-list)))))

;; --- Hand-rolled validation handlers (mimic def-op output) ---
;;
;; These dispatch on the head and validate. Recursion is explicit. This
;; matches what the def-op handlers in qbe.lisp do, just without the
;; def-op machinery.

(defun validate-qbe-form (expr)
  (cond
    ((not (consp expr))
     ;; Atoms (numbers, names, etc.) are leaves -- no validation.
     t)
    (t
     (let ((op (first expr)))
       (case op
         (:assign
          (destructuring-bind (op-kw var type opcode &rest args) expr
            (declare (ignore op-kw var))
            (check-base-type type expr ":assign")
            (check-assign-opcode opcode expr)
            (mapc #'validate-qbe-form args)))
         (:instr
          (destructuring-bind (op-kw opcode &rest args) expr
            (declare (ignore op-kw))
            (check-effect-opcode opcode expr)
            (mapc #'validate-qbe-form args)))
         (:phi
          (destructuring-bind (op-kw var type &rest args) expr
            (declare (ignore op-kw var))
            (check-base-type type expr ":phi")
            (when (oddp (length args))
              (qbe-error expr
                         ":phi expects pairs of (label value); got ~D arguments (odd)."
                         (length args)))
            (loop for (lbl val) on args by #'cddr
                  do (when lbl (validate-qbe-form lbl))
                     (when val (validate-qbe-form val)))))
         (:union-type
          (destructuring-bind (op-kw name align &rest variants) expr
            (declare (ignore op-kw name align))
            (dolist (v variants)
              (check-cons-with-head v :union expr ":union-type variant")
              (validate-qbe-form v))))
         (:union
          (destructuring-bind (op-kw &rest variants) expr
            (declare (ignore op-kw))
            (dolist (v variants)
              (check-cons-with-head v :field expr ":union variant field")
              (validate-qbe-form v))))
         (:field
          (destructuring-bind (op-kw type &optional count) expr
            (declare (ignore op-kw count))
            (check-ext-type type expr ":field")))
         (:data-item
          (destructuring-bind (op-kw type &rest vals) expr
            (declare (ignore op-kw))
            (check-ext-type type expr ":data-item")
            (when (eq type :z)
              (unless (= (length vals) 1)
                (qbe-error expr
                           ":z data-item takes exactly one size argument; got ~D."
                           (length vals))))))
         (:param
          (destructuring-bind (op-kw type &optional name) expr
            (declare (ignore op-kw name))
            (cond ((eq type :...) t)
                  ((eq type :env) t)
                  (t (check-abity type expr ":param")))))
         (:call-assign
          (destructuring-bind (op-kw var type target &rest args) expr
            (declare (ignore op-kw var))
            (check-abity type expr ":call-assign return type")
            (validate-qbe-form target)
            (dolist (a args)
              (check-cons-with-head a :call-arg expr ":call-assign argument")
              (validate-qbe-form a))))
         (:module
          (mapc #'validate-qbe-form (rest expr)))
         (:function
          (destructuring-bind (op-kw name linkage ret-type params &rest blocks) expr
            (declare (ignore op-kw name linkage))
            (when ret-type (check-abity ret-type expr ":function return type"))
            (dolist (p params)
              (check-cons-with-head p :param expr ":function param")
              (validate-qbe-form p))
            (dolist (b blocks)
              (check-cons-with-head b :block expr ":function body")
              (validate-qbe-form b))))
         (:block
          (destructuring-bind (op-kw name &rest instrs) expr
            (declare (ignore op-kw name))
            (mapc #'validate-qbe-form instrs)))
         (otherwise
          ;; Unknown op -- in the real interpreter this would be on-unknown
          ;; :error. Here we just walk children.
          (dolist (child (rest expr))
            (when (consp child) (validate-qbe-form child)))))))))

;; --- Test framework ---

(defvar *tests-run* 0) (defvar *tests-passed* 0) (defvar *failures* '())
(defmacro deftest (name &body body)
  `(progn (incf *tests-run*)
          (handler-case (progn ,@body
                               (incf *tests-passed*)
                               (format t "  PASS ~A~%" ',name))
            (error (e)
              (push (cons ',name e) *failures*)
              (format t "  FAIL ~A: ~A~%" ',name e)))))
(defun check (l e a)
  (unless (equalp e a) (error "~A: expected ~S got ~S" l e a)))
(defun check-true (l x) (unless x (error "~A: expected true" l)))
(defun signals-with-loc (form expected-line expected-substring)
  (handler-case (progn (validate-qbe-form form)
                       (error "should have signalled"))
    (qbe-validation-error (c)
      (let ((msg (qbe-validation-error-message c))
            (loc (source-loc-or-ancestor
                  (qbe-validation-error-expression c))))
        (unless (search expected-substring msg)
          (error "expected message containing ~S, got ~S"
                 expected-substring msg))
        (when expected-line
          (unless loc (error "expected loc, got nil"))
          (unless (= (source-loc-start-line loc) expected-line)
            (error "expected line ~D, got ~D"
                   expected-line (source-loc-start-line loc))))
        :ok))))

(defun run-tests ()
  (setf *tests-run* 0 *tests-passed* 0 *failures* '())

  (format t "~&=== Type predicates ===~%")
  (deftest base-types-positive
    (check-true ":w" (qbe-abity-p :w)) (check-true ":d" (qbe-abity-p :d)))
  (deftest aggregate-keyword
    (check-true "non-reserved kw is aggregate" (qbe-aggregate-type-p :myty))
    (check ":w not aggregate" nil (qbe-aggregate-type-p :w))
    (check-true "string is aggregate" (qbe-aggregate-type-p "myty"))
    (check-true "(:user-type ...) is aggregate"
                (qbe-aggregate-type-p '(:user-type "x"))))

  (format t "~&=== Validation: success cases ===~%")

  (deftest valid-assign
    (validate-qbe-form '(:assign (:temp x) :w :add 1 2)))

  (deftest valid-phi
    (validate-qbe-form
     '(:phi (:temp r) :w (:label a) (:temp x) (:label b) (:temp y))))

  (deftest valid-union-type
    (validate-qbe-form
     '(:union-type :myunion 8
       (:union (:field :w))
       (:union (:field :s)))))

  (deftest valid-empty-module (validate-qbe-form '(:module)))

  (deftest valid-full-function
    (validate-qbe-form
     '(:function (:global "main") :export :w
       ((:param :w "x"))
       (:block (:label "start")
        (:assign (:temp r) :w :add (:temp x) 1)
        (:ret (:temp r))))))

  (format t "~&=== Validation: type errors with locs ===~%")

  (deftest assign-bad-base-type
    (clear-locs)
    (let ((bad (tag (list :assign (list :temp 'x) :bogus :add 1 2)
                    "f.lisp" 17)))
      (signals-with-loc bad 17 "Invalid base type")))

  (deftest assign-bad-opcode
    (clear-locs)
    (let ((bad (tag (list :assign (list :temp 'x) :w :not-a-real-op 1 2)
                    "f.lisp" 23)))
      (signals-with-loc bad 23 "Invalid assignment opcode")))

  (deftest field-bad-ext-type
    (clear-locs)
    (let ((bad (tag (list :field :bogus) "f.lisp" 9)))
      (signals-with-loc bad 9 "Invalid extended type")))

  (deftest call-assign-bad-abity
    (clear-locs)
    ;; :... is a special type, not a valid ABITY for a call return.
    (let ((bad (tag (list :call-assign (list :temp 'r) :... (list :global "f"))
                    "f.lisp" 5)))
      (signals-with-loc bad 5 "Invalid ABITY")))

  (format t "~&=== Validation: shape errors ===~%")

  (deftest phi-odd-args
    (clear-locs)
    (let ((bad (tag (list :phi (list :temp 'r) :w
                          (list :label 'a) (list :temp 'x)
                          (list :label 'b))   ; missing the value
                    "f.lisp" 11)))
      (signals-with-loc bad 11 "expects pairs")))

  (deftest union-type-bad-variant-head
    (clear-locs)
    (let ((bad (tag (list :union-type :u 8
                          (list :not-union (list :field :w)))
                    "f.lisp" 4)))
      (signals-with-loc bad 4 "expected a (:UNION ...) form")))

  (deftest data-item-z-arity
    (clear-locs)
    (let ((bad (tag (list :data-item :z 1 2 3) "f.lisp" 8)))
      (signals-with-loc bad 8 ":z data-item takes exactly one")))

  (deftest function-bad-param-shape
    (clear-locs)
    (let ((bad (tag (list :function (list :global "f") nil :w
                          ;; Param should be (:param ...), not bare cons.
                          (list (list :w "x"))
                          (list :block (list :label "s") (list :ret)))
                    "f.lisp" 14)))
      (signals-with-loc bad 14 "expected a (:PARAM ...) form")))

  (deftest call-assign-bad-arg-shape
    (clear-locs)
    (let ((bad (tag (list :call-assign (list :temp 'r) :w (list :global "f")
                          ;; arg should be (:call-arg :w x), not bare
                          (list :w "x"))
                    "f.lisp" 6)))
      (signals-with-loc bad 6 "expected a (:CALL-ARG ...) form")))

  (format t "~&=== Errors propagate from deep within nested forms ===~%")

  (deftest nested-error-points-at-deepest-loc
    ;; The validator should signal at the *inner* form, not the outer
    ;; one, when the error is in a sub-expression.
    (clear-locs)
    (let* ((bad-assign (tag (list :assign (list :temp 'r) :nope :add 1 2)
                            "f.lisp" 99))
           (block-form (tag (list :block (list :label "s") bad-assign)
                            "f.lisp" 50))
           (mod (tag (list :module
                           (tag (list :function (list :global "g") nil :w nil
                                      block-form)
                                "f.lisp" 30))
                     "f.lisp" 1)))
      ;; The error should report line 99 (the bad :assign), not the
      ;; outer :module or :function or :block.
      (signals-with-loc mod 99 "Invalid base type")))

  (format t "~&=== format-linkage uniformity ===~%")

  (deftest format-linkage-empty (check "nil" "" (format-linkage nil)))
  (deftest format-linkage-single-symbol
    (check ":export" "export " (format-linkage :export)))
  (deftest format-linkage-list-of-symbols
    (check "list lowercased" "export thread "
           (format-linkage '(:export :thread))))
  (deftest format-linkage-mixed-with-string
    ;; Strings (e.g. section names) are left intact; symbols downcase.
    (check "section name preserved"
           "section \"data\" "
           (format-linkage '(:section "\"data\""))))

  (format t "~&----~%~D/~D tests passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f *failures*) (format t "  ~A: ~A~%" (car f) (cdr f))))
  (= *tests-passed* *tests-run*))

(unless (run-tests) (sb-ext:exit :code 1))
