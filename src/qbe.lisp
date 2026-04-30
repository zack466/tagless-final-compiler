(in-package #:tagless-compiler)

;; --- QBE language ---
;;
;; Two interpreters:
;;
;;   *qbe-validate*  walks the AST and signals QBE-VALIDATION-ERROR on
;;                   any shape or type violation. Source locations are
;;                   looked up in error reports. The validator does not
;;                   produce a useful return value; it's run for its
;;                   side effect (signaling).
;;
;;   *qbe*           prints a validated AST to a string. It still calls
;;                   the validation helpers as a safety net, so running
;;                   the printer on un-validated AST fails loudly with
;;                   the same source-loc-aware error rather than
;;                   silently producing bad output.
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
;; location info to errors. Pass (expr) inside a def-op body, or the
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

(defparameter *qbe-validate*
  (make-interpreter :on-unknown :error
                    :propagate-source-locations nil))

;; --- Sigils ---

(def-op *qbe-validate* (:global name &optional offset)
  t)

(def-op *qbe-validate* (:thread name) t)
(def-op *qbe-validate* (:temp name)   t)
(def-op *qbe-validate* (:label name)  t)
(def-op *qbe-validate* (:user-type name) t)

;; --- Top-level declarations ---

(def-op *qbe-validate* (:module &rest decls)
  (mapc #'recurse decls)
  t)

(def-op *qbe-validate* (:type name align &rest fields)
  (dolist (f fields)
    (check-cons-with-head f :field (expr) ":type field")
    (recurse f))
  t)

(def-op *qbe-validate* (:opaque name align size)
  t)

(def-op *qbe-validate* (:union-type name align &rest variants)
  (dolist (v variants)
    (check-cons-with-head v :union (expr) ":union-type variant")
    (recurse v))
  t)

(def-op *qbe-validate* (:union &rest variants)
  (dolist (v variants)
    (check-cons-with-head v :field (expr) ":union variant field")
    (recurse v))
  t)

(def-op *qbe-validate* (:data name linkage align &rest items)
  (dolist (item items)
    (check-cons-with-head item :data-item (expr) ":data item")
    (recurse item))
  t)

(def-op *qbe-validate* (:function name linkage ret-type params &rest blocks)
  ;; ret-type may be nil (void) or any ABITY.
  (when ret-type
    (check-abity ret-type (expr) ":function return type"))
  (dolist (p params)
    (check-cons-with-head p :param (expr) ":function param")
    (recurse p))
  (dolist (b blocks)
    (check-cons-with-head b :block (expr) ":function body")
    (recurse b))
  t)

(def-op *qbe-validate* (:field type &optional count)
  (check-ext-type type (expr) ":field")
  t)

(def-op *qbe-validate* (:data-item type &rest vals)
  (check-ext-type type (expr) ":data-item")
  (when (eq type :z)
    (unless (= (length vals) 1)
      (qbe-error (expr)
                 ":z data-item takes exactly one size argument; got ~D."
                 (length vals))))
  t)

(def-op *qbe-validate* (:param type &optional name)
  (cond ((eq type :...) t)
        ((eq type :env) t)
        (t (check-abity type (expr) ":param")))
  t)

;; --- Blocks and control flow ---

(def-op *qbe-validate* (:block name &rest instrs)
  (mapc #'recurse instrs)
  t)

(def-op *qbe-validate* (:jmp label) (recurse label) t)

(def-op *qbe-validate* (:jnz val label-true label-false)
  (recurse val)
  (recurse label-true)
  (recurse label-false)
  t)

(def-op *qbe-validate* (:ret &optional val)
  (when val (recurse val))
  t)

(def-op *qbe-validate* (:hlt) t)

;; --- Instructions ---

(def-op *qbe-validate* (:assign var type op &rest args)
  (check-base-type type (expr) ":assign")
  (check-assign-opcode op (expr))
  (mapc #'recurse args)
  t)

(def-op *qbe-validate* (:instr op &rest args)
  (check-effect-opcode op (expr))
  (mapc #'recurse args)
  t)

;; --- Calls ---

(def-op *qbe-validate* (:call-assign var type target &rest args)
  (check-abity type (expr) ":call-assign return type")
  (recurse target)
  (dolist (a args)
    (check-cons-with-head a :call-arg (expr) ":call-assign argument")
    (recurse a))
  t)

(def-op *qbe-validate* (:call-arg type val)
  (cond ((eq type :...) t)
        ((eq type :env) (recurse val))
        (t (check-abity type (expr) ":call-arg")
           (recurse val)))
  t)

(def-op *qbe-validate* (:call target &rest args)
  (recurse target)
  (dolist (a args)
    (check-cons-with-head a :call-arg (expr) ":call argument")
    (recurse a))
  t)

;; --- Phi ---

(def-op *qbe-validate* (:phi var type &rest args)
  (check-base-type type (expr) ":phi")
  (when (oddp (length args))
    (qbe-error (expr)
               ":phi expects pairs of (label value); got ~D arguments (odd)."
               (length args)))
  ;; Each pair: (label-form value-form). Recurse on both halves.
  (loop for (lbl val) on args by #'cddr
        do (when lbl (recurse lbl))
           (when val (recurse val)))
  t)

(defun validate-qbe (ast)
  "Validate AST. Signals QBE-VALIDATION-ERROR on the first violation
   found. Returns AST on success (so this can be used in a pipeline)."
  (lower *qbe-validate* ast)
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
  (check-ext-type type (expr) ":field")
  (if count
      (format nil "~(~a~) ~a" type count)
      (format nil "~(~a~)" type)))

(def-op *qbe* (:data-item type &rest vals)
  (check-ext-type type (expr) ":data-item")
  (if (eq type :z)
      (progn
        (unless (= (length vals) 1)
          (qbe-error (expr)
                     ":z data-item takes exactly one size argument; got ~D."
                     (length vals)))
        (format nil "z ~a" (first vals)))
      (format nil "~(~a~) ~{~a~^ ~}" type (mapcar #'recurse vals))))

(def-op *qbe* (:param type &optional name)
  (cond ((eq type :...) "...")
        ((eq type :env) (format nil "env ~a" (recurse name)))
        (t (check-abity type (expr) ":param")
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
  (check-base-type type (expr) ":assign")
  (check-assign-opcode op (expr))
  (format nil "~a =~(~a~) ~(~a~) ~{~a~^, ~}"
          (recurse var) type op (mapcar #'recurse args)))

(def-op *qbe* (:instr op &rest args)
  (check-effect-opcode op (expr))
  (format nil "~(~a~) ~{~a~^, ~}" op (mapcar #'recurse args)))

;; --- Calls ---

(def-op *qbe* (:call-assign var type target &rest args)
  (check-abity type (expr) ":call-assign return type")
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
        (t (check-abity type (expr) ":call-arg")
           (if (qbe-aggregate-type-p type)
               (format nil "~a ~a" (recurse type) (recurse val))
               (format nil "~(~a~) ~a" type (recurse val))))))

(def-op *qbe* (:call target &rest args)
  (format nil "call ~a(~{~a~^, ~})"
          (recurse target) (mapcar #'recurse args)))

;; --- Phi ---

(def-op *qbe* (:phi var type &rest args)
  (check-base-type type (expr) ":phi")
  (when (oddp (length args))
    (qbe-error (expr)
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
