(in-package #:tagless-compiler)

;; --- QBE language ---
;;
;; AST generation and validation pipeline:
;;
;;   *qbe-grammar*   A grammar system specification that ensures AST nodes
;;                   match exactly the layout and shapes QBE expects.
;;                   `validate-qbe` runs the AST through this grammar and
;;                   signals `match-error` on violations.
;;
;;   *qbe*           An interpreter that prints a validated AST to a string.
;;                   It still calls basic validation helpers as a safety net, 
;;                   so running the printer on un-validated AST fails loudly
;;                   with source-loc-aware errors rather than silently
;;                   producing bad output.
;;
;; The intended pipeline is:
;;
;;   (validate-qbe ast)            ; signals if invalid
;;   (lower *qbe* ast)             ; produces the QBE IL string
;;
;; Or just call BUILD-QBE-AST, which does both.

;; -----------------------------------------------------------------------------
;; Validation condition
;; -----------------------------------------------------------------------------
;; TODO: define a macro to more easily define errors specific to interpreters,
;; automatically print out context and stuff.

(define-condition qbe-validation-error (error)
  ((message    :initarg :message    :reader qbe-validation-error-message)
   (expression :initarg :expression :reader qbe-validation-error-expression))
  (:report
   (lambda (c stream)
     (let ((loc (source-loc-or-ancestor
                 (qbe-validation-error-expression c))))
       (format stream "~A~@[~%  at: ~A~]"
               (qbe-validation-error-message c)
               (when loc (format-source-loc loc)))
       (when loc
         (print-source-context loc :stream stream))))))

(defun qbe-error (expression fmt &rest args)
  "Signal a QBE-VALIDATION-ERROR. EXPRESSION is the offending form
   (used for source-loc lookup in the report); FMT and ARGS are the
   message."
  (error 'qbe-validation-error
         :message (apply #'format nil fmt args)
         :expression expression))

;; -----------------------------------------------------------------------------
;; Type tables
;; -----------------------------------------------------------------------------

(defparameter *qbe-base-types*    '(:w :l :s :d))
(defparameter *qbe-subw-types*    '(:sb :ub :sh :uh))
(defparameter *qbe-ext-types*     '(:w :l :s :d :b :h :z))
(defparameter *qbe-special-types* '(:... :env))

(defparameter *qbe-assign-opcodes*
  '(:add :sub :div :mul :neg :udiv :rem :urem
    :or :xor :and :sar :shr :shl
    :alloc4 :alloc8 :alloc16
    :loadd :loads :loadl :loadsw :loaduw
    :loadsh :loaduh :loadsb :loadub :loadw
    :ceqd :ceql :ceqs :ceqw :cged :cges :cgtd :cgts
    :cled :cles :cltd :clts :cned :cnel :cnes :cnew
    :cod :cos :csgel :csgew :csgtl :csgtw :cslel :cslew
    :csltl :csltw :cugel :cugew :cugtl :cugtw :culel
    :culew :cultl :cultw :cuod :cuos
    :dtosi :dtoui :exts :extsb :extsh :extsw :extub
    :extuh :extuw :sltof :ultof :stosi :stoui :swtof
    :uwtof :truncd
    :cast :copy :vaarg))

(defparameter *qbe-effect-opcodes*
  '(:storeb :stored :storeh :storel :stores :storew
    :blit :vastart))

;; -----------------------------------------------------------------------------
;; QBE AST Grammar
;; -----------------------------------------------------------------------------

(defparameter *qbe-grammar*
  `((:module
     (repeat0 (option :type :opaque :union-type :data :function)))

    (:type
     :user-type (any) (repeat0 :field))

    (:opaque
     :user-type (any) (any))

    (:union-type
     :user-type (any) (repeat0 :union))

    (:union
     (repeat0 :field))

    (:data
     (identifier) (any) (any) (repeat0 :data-item))

    (:function
     (identifier) (any) (maybe :abity) (repeat0 :param) (repeat0 :block))

    (:field
     :ext-type (maybe (any)))

    (:data-item
     :ext-type (repeat0 (any)))

    (:param
     (option
      ((keyword :...))
      ((keyword :env) (any))
      (:abity (maybe (any)))))

    (:block
     :label (repeat0 (option :assign :instr :call-assign :call :phi))
     (option :jmp :jnz :ret :hlt))

    (:jmp :label)
    (:jnz :value :label :label)
    (:ret (maybe :value))
    (:hlt)

    (:assign
     :temp :base-type :assign-op (repeat0 :value))

    (:instr
     :effect-op (repeat0 :value))

    (:call-assign
     :temp :abity :value (repeat0 :call-arg))

    (:call
     :value (repeat0 :call-arg))

    (:call-arg
     (option
      ((keyword :...))
      ((keyword :env) :value)
      (:abity :value)))

    (:phi
     :temp :base-type (repeat0 :phi-arg))

    (:phi-arg
     :label :value)

    (:label (option (:label (any))))
    (:temp (option (:temp (any))))
    (:user-type (option (:user-type (any))))
    (:value
     (option
      (:temp (any))
      (:global (any) (maybe (any)))
      (:thread (any))
      (literal)))

    (:abity
     (option
      (dispatch (option ,@(mapcar (lambda (x) `(keyword ,x)) *qbe-base-types*)))
      (dispatch (option ,@(mapcar (lambda (x) `(keyword ,x)) *qbe-subw-types*)))
      :user-type
      (string)
      (identifier)))

    (:base-type
     (dispatch (option ,@(mapcar (lambda (x) `(keyword ,x)) *qbe-base-types*))))

    (:ext-type
     (dispatch (option ,@(mapcar (lambda (x) `(keyword ,x)) *qbe-ext-types*))))

    (:assign-op
     (dispatch (option ,@(mapcar (lambda (x) `(keyword ,x)) *qbe-assign-opcodes*))))

    (:effect-op
     (dispatch (option ,@(mapcar (lambda (x) `(keyword ,x)) *qbe-effect-opcodes*))))))

;; -----------------------------------------------------------------------------
;; Type predicates
;; -----------------------------------------------------------------------------

(defun qbe-aggregate-type-p (x)
  "Aggregate type: a (:user-type \"name\") form, a string, or a keyword
   that isn't one of the reserved type keywords."
  (or (stringp x)
      (and (consp x) (eq (first x) :user-type))
      (and (keywordp x)
           (not (member x *qbe-base-types* :test #'eq))
           (not (member x *qbe-subw-types* :test #'eq))
           (not (member x *qbe-ext-types* :test #'eq))
           (not (member x *qbe-special-types* :test #'eq)))))

(defun qbe-abity-p (x)
  "ABITY = base | sub-word | aggregate."
  (or (member x *qbe-base-types* :test #'eq)
      (member x *qbe-subw-types* :test #'eq)
      (qbe-aggregate-type-p x)))

;; -----------------------------------------------------------------------------
;; Validation helpers
;;
;; Each helper takes an EXPRESSION argument used purely to attach source
;; location info to errors. Pass (this) inside a def-op body, or the
;; specific sub-cons that's wrong if you have one.
;; -----------------------------------------------------------------------------

(defun check-base-type (type expression context)
  (unless (member type *qbe-base-types* :test #'eq)
    (qbe-error expression
               "Invalid base type ~S in ~A. Expected one of ~S."
               type context *qbe-base-types*)))

(defun check-ext-type (type expression context)
  (unless (member type *qbe-ext-types* :test #'eq)
    (qbe-error expression
               "Invalid extended type ~S in ~A. Expected one of ~S."
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
  "Verify FORM is a cons whose CAR is EQ to HEAD. CONTEXT is a string
   describing where the form appears (for the error message)."
  (unless (and (consp form) (eq (first form) head))
    (qbe-error expression
               "~A: expected a (~S ...) form, got ~S."
               context head form)))

;; -----------------------------------------------------------------------------
;; Linkage formatting (used by both passes)
;; -----------------------------------------------------------------------------

(defun format-linkage-flag (flag)
  "Format a single linkage flag, downcasing keywords/symbols and
   leaving strings alone (for things like section names)."
  (cond ((stringp flag) flag)
        ((or (keywordp flag) (symbolp flag))
         (format nil "~(~a~)" flag))
        (t (format nil "~a" flag))))

(defun format-linkage (linkage-list)
  "LINKAGE* -- zero or more flags. Accepts nil, a single flag, or a list
   of flags. Returns a string with a trailing space if non-empty, or
   the empty string."
  (cond ((null linkage-list) "")
        ((atom linkage-list)
         (format nil "~a " (format-linkage-flag linkage-list)))
        (t
         (format nil "~{~a~^ ~} "
                 (mapcar #'format-linkage-flag linkage-list)))))

;; -----------------------------------------------------------------------------
;; Validation interpreter
;;
;; Every handler follows the same shape: validate the immediate form's
;; types and shape, then recurse on sub-expressions that themselves are
;; AST forms (operators, blocks, instructions, params, ...). Leaf values
;; like names, sizes, and offsets aren't recursed. The validator returns
;; T from successful checks; the return value is unused.
;; -----------------------------------------------------------------------------

(defun validate-qbe (ast)
  "Validate AST using the QBE grammar. Signals MATCH-ERROR on violation.
   Returns AST on success."
  (match-grammar ast :module *qbe-grammar*)
  ast)

;; -----------------------------------------------------------------------------
;; Printer
;;
;; The printer assumes its input has been validated, but still calls the
;; validation helpers as a safety net. If you skip validation and the
;; AST is malformed, you'll still get a QBE-VALIDATION-ERROR with source
;; location info -- the printer just won't produce a string in that
;; case.
;; -----------------------------------------------------------------------------

(defparameter *qbe* (make-interpreter
                     :on-unknown :error
                     ;; Output is strings, so loc propagation is a no-op
                     ;; -- skip it for clarity.
                     :propagate-source-locations nil
                     :readable-name "QBE"))

;; --- Sigils ---

(def-op *qbe* (:global name &optional offset)
  (if offset
      (format nil "$~a + ~a" name offset)
      (format nil "$~a" name)))

(def-op *qbe* (:thread name)    (format nil "thread $~a" name))
(def-op *qbe* (:temp name)      (format nil "%~a" name))
(def-op *qbe* (:label name)     (format nil "@~a" name))
(def-op *qbe* (:user-type name) (format nil ":~a" name))

;; --- Top-level declarations ---

(def-op *qbe* (:module &rest decls)
  (format nil "~{~a~^~%~%~}" (mapcar #'recurse decls)))

(def-op *qbe* (:type name align &rest fields)
  (format nil "type ~a = ~@[align ~a ~]{ ~{~a~^, ~} }"
          (recurse name) align (mapcar #'recurse fields)))

(def-op *qbe* (:opaque name align size)
  (format nil "type ~a = align ~a { ~a }" (recurse name) align size))

(def-op *qbe* (:union-type name align &rest variants)
  (format nil "type ~a = ~@[align ~a ~]{ ~{~a~^ ~} }"
          (recurse name)
          align
          (mapcar (lambda (v)
                    ;; Validator already enforced (:union ...) head, but
                    ;; re-check defensively.
                    (check-cons-with-head v :union v ":union-type variant")
                    (format nil "{ ~{~a~^, ~} }"
                            (mapcar #'recurse (rest v))))
                  variants)))

(def-op *qbe* (:union &rest variants)
  (format nil "{ ~{~a~^ ~} }"
          (mapcar (lambda (v) (format nil "{ ~a }" (recurse v)))
                  variants)))

(def-op *qbe* (:data name linkage align &rest items)
  (format nil "~adata ~a = ~@[align ~a ~]{ ~{~a~^, ~} }"
          (format-linkage linkage)
          (recurse name)
          align
          (mapcar #'recurse items)))

(def-op *qbe* (:function name linkage ret-type params &rest blocks)
  (format nil "~afunction ~@[~(~a~) ~]~a(~{~a~^, ~}) {~%~{~a~^~%~}~%}"
          (format-linkage linkage)
          ret-type
          (recurse name)
          (mapcar #'recurse params)
          (mapcar #'recurse blocks)))

(def-op *qbe* (:field type &optional count)
  (check-ext-type type (this) ":field")
  (if count
      (format nil "~(~a~) ~a" type count)
      (format nil "~(~a~)" type)))

(def-op *qbe* (:data-item type &rest vals)
  (check-ext-type type (this) ":data-item")
  (if (eq type :z)
      (progn
        (unless (= (length vals) 1)
          (qbe-error (this)
                     ":z data-item takes exactly one size argument; got ~D."
                     (length vals)))
        (format nil "z ~a" (first vals)))
      (format nil "~(~a~) ~{~a~^ ~}" type (mapcar #'recurse vals))))

(def-op *qbe* (:param type &optional name)
  (cond ((eq type :...) "...")
        ((eq type :env) (format nil "env ~a" (recurse name)))
        (t (check-abity type (this) ":param")
           (if (qbe-aggregate-type-p type)
               (format nil "~a ~a" (recurse type) (recurse name))
               (format nil "~(~a~) ~a" type (recurse name))))))

;; --- Blocks and control flow ---

(def-op *qbe* (:block name &rest instrs)
  (format nil "~a~%~{        ~a~^~%~}"
          (recurse name) (mapcar #'recurse instrs)))

(def-op *qbe* (:jmp label)
  (format nil "jmp ~a" (recurse label)))

(def-op *qbe* (:jnz val label-true label-false)
  (format nil "jnz ~a, ~a, ~a"
          (recurse val) (recurse label-true) (recurse label-false)))

(def-op *qbe* (:ret &optional val)
  (if val (format nil "ret ~a" (recurse val)) "ret"))

(def-op *qbe* (:hlt) "hlt")

;; --- Instructions ---

(def-op *qbe* (:assign var type op &rest args)
  (check-base-type type (this) ":assign")
  (check-assign-opcode op (this))
  (format nil "~a =~(~a~) ~(~a~) ~{~a~^, ~}"
          (recurse var) type op (mapcar #'recurse args)))

(def-op *qbe* (:instr op &rest args)
  (check-effect-opcode op (this))
  (format nil "~(~a~) ~{~a~^, ~}" op (mapcar #'recurse args)))

;; --- Calls ---

(def-op *qbe* (:call-assign var type target &rest args)
  (check-abity type (this) ":call-assign return type")
  (if (qbe-aggregate-type-p type)
      (format nil "~a =~a call ~a(~{~a~^, ~})"
              (recurse var) (recurse type) (recurse target)
              (mapcar #'recurse args))
      (format nil "~a =~(~a~) call ~a(~{~a~^, ~})"
              (recurse var) type (recurse target)
              (mapcar #'recurse args))))

(def-op *qbe* (:call-arg type val)
  (cond ((eq type :...) "...")
        ((eq type :env) (format nil "env ~a" (recurse val)))
        (t (check-abity type (this) ":call-arg")
           (if (qbe-aggregate-type-p type)
               (format nil "~a ~a" (recurse type) (recurse val))
               (format nil "~(~a~) ~a" type (recurse val))))))

(def-op *qbe* (:call target &rest args)
  (format nil "call ~a(~{~a~^, ~})"
          (recurse target) (mapcar #'recurse args)))

;; --- Phi ---

(def-op *qbe* (:phi var type &rest args)
  (check-base-type type (this) ":phi")
  (when (oddp (length args))
    (qbe-error (this)
               ":phi expects pairs of (label value); got ~D arguments."
               (length args)))
  (let ((pairs (loop for (lbl val) on args by #'cddr
                     collect (format nil "~a ~a" (recurse lbl) (recurse val)))))
    (format nil "~a =~(~a~) phi ~{~a~^, ~}" (recurse var) type pairs)))

;; -----------------------------------------------------------------------------
;; Building
;; -----------------------------------------------------------------------------

(defun build-qbe-ast (ast &key
                            (out-name "program")
                            (runtime-c "runtime.c")
                            (keep-temp-files t)
                            (validate t))
  "Compiles a QBE AST into an executable using `qbe` and `clang`. If
   VALIDATE is true (the default), runs the validation pass first;
   on validation failure, signals QBE-VALIDATION-ERROR with source
   location info before any files are written."
  (when validate
    (validate-qbe ast))
  (let ((ssa-file (format nil "~a.ssa" out-name))
        (asm-file (format nil "~a.s" out-name))
        (il-string (lower *qbe* ast)))

    (format t ";; Writing IL to ~S~%" ssa-file)
    (with-open-file (stream ssa-file :direction :output :if-exists :supersede)
      (write-string il-string stream))

    (format t ";; Running QBE: qbe -o ~a ~a~%" asm-file ssa-file)
    (handler-case
        (uiop:run-program (list "qbe" "-o" asm-file ssa-file)
                          :output *standard-output*
                          :error-output *error-output*)
      (error (e) (error "QBE compilation failed: ~a" e)))

    (format t ";; Running Clang: clang -o ~a ~a ~a~%" out-name asm-file runtime-c)
    (handler-case
        (uiop:run-program (list "clang" "-o" out-name asm-file runtime-c)
                          :output *standard-output*
                          :error-output *error-output*)
      (error (e) (error "Clang linking failed: ~a" e)))

    (unless keep-temp-files
      (delete-file ssa-file)
      (delete-file asm-file)
      (format t ";; Cleaned up intermediate files.~%"))

    (format t ";; Build complete! Executable created: ./~a~%" out-name)
    out-name))
