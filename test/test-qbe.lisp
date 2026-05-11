(in-package #:tagless-compiler)

;;; Tests for the QBE validation helpers in qbe.lisp.
;;; Uses a local walk-and-validate helper (validate-qbe-form) that directly
;;; calls the real check-* functions and signals qbe-validation-error.
;;; This exercises the validation logic independently of the printer.

;;; --- Test-local walker ---

(defun validate-qbe-form (expr)
  "Walk EXPR recursively, calling the real check-* helpers from qbe.lisp.
   Signals QBE-VALIDATION-ERROR on the first invalid form found."
  (when (consp expr)
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
             (qbe-error expr ":phi expects pairs of (label value); got ~D arguments (odd)."
                        (length args)))
           (loop for (lbl val) on args by #'cddr
                 do (when lbl (validate-qbe-form lbl))
                    (when val  (validate-qbe-form val)))))
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
               (qbe-error expr ":z data-item takes exactly one size argument; got ~D."
                          (length vals))))))
        (:param
         (destructuring-bind (op-kw type &optional name) expr
           (declare (ignore op-kw name))
           (cond ((eq type :...)  t)
                 ((eq type :env)  t)
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
         (dolist (child (rest expr))
           (when (consp child) (validate-qbe-form child))))))))

;;; --- Helpers ---

(defun make-qbe-loc (file line)
  (make-source-loc :file file :start-line line :start-col 1
                   :end-line line :end-col 9
                   :start-offset (* line 100)
                   :end-offset   (+ (* line 100) 9)))

(defun tag-qbe (form file line)
  (setf (source-loc form) (make-qbe-loc file line))
  form)

(defun qbe-signals-with-loc (form expected-line expected-substring)
  "Call validate-qbe-form on FORM; expect it to signal QBE-VALIDATION-ERROR
   whose message contains EXPECTED-SUBSTRING and whose expression has a loc
   on EXPECTED-LINE (or nil to skip the line check)."
  (handler-case
      (progn (validate-qbe-form form)
             (error "Expected QBE-VALIDATION-ERROR but no condition was signalled."))
    (qbe-validation-error (c)
      (let ((msg (qbe-validation-error-message c))
            (loc (source-loc-or-ancestor (qbe-validation-error-expression c))))
        (unless (search expected-substring msg)
          (error "Expected message containing ~S, got: ~S" expected-substring msg))
        (when expected-line
          (unless loc
            (error "Expected source-loc but got NIL"))
          (unless (= (source-loc-start-line loc) expected-line)
            (error "Expected error at line ~D, got line ~D"
                   expected-line (source-loc-start-line loc))))
        :ok))))

;;; ---------------------------------------------------------------------------
;;; Suite: type predicates
;;; ---------------------------------------------------------------------------

(defsuite "qbe: type predicates"
  (deftest "base types are abity"
    (check-true (qbe-abity-p :w))
    (check-true (qbe-abity-p :d)))

  (deftest "non-reserved keyword is aggregate"
    (check-true (qbe-aggregate-type-p :myty)))

  (deftest "reserved keyword is not aggregate"
    (check-false (qbe-aggregate-type-p :w)))

  (deftest "string is aggregate"
    (check-true (qbe-aggregate-type-p "myty")))

  (deftest "(:user-type ...) is aggregate"
    (check-true (qbe-aggregate-type-p '(:user-type "x")))))

;;; ---------------------------------------------------------------------------
;;; Suite: valid forms
;;; ---------------------------------------------------------------------------

(defsuite "qbe: valid forms"
  (deftest "valid :assign"
    (validate-qbe-form '(:assign (:temp x) :w :add 1 2)))

  (deftest "valid :phi"
    (validate-qbe-form
     '(:phi (:temp r) :w (:label a) (:temp x) (:label b) (:temp y))))

  (deftest "valid :union-type"
    (validate-qbe-form
     '(:union-type :myunion 8
       (:union (:field :w))
       (:union (:field :s)))))

  (deftest "valid empty :module"
    (validate-qbe-form '(:module)))

  (deftest "valid :function"
    (validate-qbe-form
     '(:function (:global "main") :export :w
       ((:param :w "x"))
       (:block (:label "start")
        (:assign (:temp r) :w :add (:temp x) 1)
        (:ret (:temp r)))))))

;;; ---------------------------------------------------------------------------
;;; Suite: type errors with source locs
;;; ---------------------------------------------------------------------------

(defsuite "qbe: type errors"
  (deftest ":assign bad base type reports correct line"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :assign (list :temp 'x) :bogus :add 1 2)
                        "f.lisp" 17)))
      (qbe-signals-with-loc bad 17 "Invalid base type")))

  (deftest ":assign bad opcode reports correct line"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :assign (list :temp 'x) :w :not-a-real-op 1 2)
                        "f.lisp" 23)))
      (qbe-signals-with-loc bad 23 "Invalid assignment opcode")))

  (deftest ":field bad ext type reports correct line"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :field :bogus) "f.lisp" 9)))
      (qbe-signals-with-loc bad 9 "Invalid extended type")))

  (deftest ":call-assign bad abity reports correct line"
    (clear-source-locations)
    ;; :... is a special type, not a valid ABITY for a return value.
    (let ((bad (tag-qbe (list :call-assign (list :temp 'r) :... (list :global "f"))
                        "f.lisp" 5)))
      (qbe-signals-with-loc bad 5 "Invalid ABITY"))))

;;; ---------------------------------------------------------------------------
;;; Suite: shape errors
;;; ---------------------------------------------------------------------------

(defsuite "qbe: shape errors"
  (deftest ":phi with odd arg count"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :phi (list :temp 'r) :w
                              (list :label 'a) (list :temp 'x)
                              (list :label 'b))  ; missing value
                        "f.lisp" 11)))
      (qbe-signals-with-loc bad 11 "expects pairs")))

  (deftest ":union-type with bad variant head"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :union-type :u 8
                              (list :not-union (list :field :w)))
                        "f.lisp" 4)))
      (qbe-signals-with-loc bad 4 "expected a (:UNION ...) form")))

  (deftest ":data-item :z with wrong arity"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :data-item :z 1 2 3) "f.lisp" 8)))
      (qbe-signals-with-loc bad 8 ":z data-item takes exactly one")))

  (deftest ":function with bad param shape"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :function (list :global "f") nil :w
                              (list (list :w "x"))   ; should be (:param ...)
                              (list :block (list :label "s") (list :ret)))
                        "f.lisp" 14)))
      (qbe-signals-with-loc bad 14 "expected a (:PARAM ...) form")))

  (deftest ":call-assign with bad arg shape"
    (clear-source-locations)
    (let ((bad (tag-qbe (list :call-assign (list :temp 'r) :w (list :global "f")
                              (list :w "x"))   ; should be (:call-arg ...)
                        "f.lisp" 6)))
      (qbe-signals-with-loc bad 6 "expected a (:CALL-ARG ...) form")))

  (deftest "error in deep nested form reports innermost line"
    (clear-source-locations)
    (let* ((bad-assign  (tag-qbe (list :assign (list :temp 'r) :nope :add 1 2)
                                 "f.lisp" 99))
           (block-form  (tag-qbe (list :block (list :label "s") bad-assign)
                                 "f.lisp" 50))
           (mod         (tag-qbe (list :module
                                       (tag-qbe
                                        (list :function (list :global "g") nil :w nil
                                              block-form)
                                        "f.lisp" 30))
                                 "f.lisp" 1)))
      (qbe-signals-with-loc mod 99 "Invalid base type"))))

;;; ---------------------------------------------------------------------------
;;; Suite: format-linkage
;;; ---------------------------------------------------------------------------

(defsuite "qbe: format-linkage"
  (deftest "nil → empty string"
    (check (format-linkage nil) ""))

  (deftest "single keyword → downcased with trailing space"
    (check (format-linkage :export) "export "))

  (deftest "list of keywords → space-separated with trailing space"
    (check (format-linkage '(:export :thread)) "export thread "))

  (deftest "string in list → preserved as-is"
    (check (format-linkage '(:section "\"data\"")) "section \"data\" ")))
