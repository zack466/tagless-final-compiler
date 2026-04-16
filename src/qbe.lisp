(in-package #:tagless-compiler)

;; Base QBE language
(defparameter *qbe* (make-interpreter))

;; Types
(defparameter *qbe-base-types* '(:w :l :s :d))
(defparameter *qbe-ext-types* '(:w :l :s :d :b :h :z))
(defparameter *qbe-special-types* '(:... :env))

;; Assignment Opcodes (Returns a value)
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

;; Effectful Opcodes (No return value)
(defparameter *qbe-effect-opcodes*
  '(:storeb :stored :storeh :storel :stores :storew
    :blit :vastart))

;; -----------------------------------------------------------------------------
;; Sigils
;; -----------------------------------------------------------------------------

;; Usage: (:global "foo" 8) -> "$foo + 8"
(def-op *qbe* :global (name &optional offset)
  (if offset
      (format nil "$~a + ~a" name offset)
      (format nil "$~a" name)))

(def-op *qbe* :thread (name)
  (format nil "thread $~a" name))

(def-op *qbe* :temp (name)
  (format nil "%~a" name))

(def-op *qbe* :label (name)
  (format nil "@~a" name))

(def-op *qbe* :user-type (name)
  (format nil ":~a" name))

;; -----------------------------------------------------------------------------
;; Top-Level Declarations
;; -----------------------------------------------------------------------------

(def-op *qbe* :module (&rest decls)
  (format nil "~{~a~^~%~%~}" (mapcar #'lower decls)))

(def-op *qbe* :type (name align &rest fields)
  (format nil "type ~a = ~@[align ~a ~]{ ~{~a~^, ~} }"
          (lower name) align (mapcar #'lower fields)))

;; Usage: (:opaque ":name" 16 32)
(def-op *qbe* :opaque (name align size)
  (format nil "type ~a = align ~a { ~a }" (lower name) align size))

(def-op *qbe* :field (type &optional count)
  (if count
      (format nil "~(~a~) ~a" type count)
      (format nil "~(~a~)" type)))

(defun format-linkage (linkage-list)
  (if linkage-list
      (format nil "~{~a~^ ~} " (if (listp linkage-list) linkage-list (list linkage-list)))
      ""))

;; Update :data and :function to use this:
(def-op *qbe* :data (name linkage align &rest items)
  (format nil "~adata ~a = ~@[align ~a ~]{ ~{~a~^, ~} }"
          (format-linkage linkage) (lower name) align (mapcar #'lower items)))

(def-op *qbe* :function (name linkage ret-type params &rest blocks)
  (format nil "~afunction ~@[~(~a~) ~]~a(~{~a~^, ~}) {~%~{~a~^~%~}~%}"
          (format-linkage linkage) ret-type (lower name)
          (mapcar #'lower params)
          (mapcar #'lower blocks)))

(def-op *qbe* :data-item (type &rest vals)
  (if (string-equal (string type) "z")
      (format nil "z ~a" (first vals))
      (format nil "~(~a~) ~{~a~^ ~}" type vals)))

(def-op *qbe* :param (type name)
  (cond ((string-equal (string type) "...") "...")
        ((string-equal (string type) "env") (format nil "env ~a" (lower name)))
        (t (format nil "~(~a~) ~a" type (lower name)))))

;; Usage: (:union (:field b) (:field s))
(def-op *qbe* :union (&rest variants)
  (format nil "{ ~{~a~^ ~} }" 
          (mapcar (lambda (v) (format nil "{ ~a }" (lower v))) 
                  variants)))

;; -----------------------------------------------------------------------------
;; Blocks and Control Flow
;; -----------------------------------------------------------------------------

(def-op *qbe* :block (name &rest instrs)
  (format nil "~a~%~{        ~a~^~%~}"
          (lower name) (mapcar #'lower instrs)))

(def-op *qbe* :jmp (label)
  (format nil "jmp ~a" (lower label)))

(def-op *qbe* :jnz (val label-true label-false)
  (format nil "jnz ~a, ~a, ~a"
          (lower val) (lower label-true) (lower label-false)))

(def-op *qbe* :ret (&optional val)
  (if val
      (format nil "ret ~a" (lower val))
      "ret"))

(def-op *qbe* :hlt ()
  "hlt")

;; -----------------------------------------------------------------------------
;; Instructions (Now with strict keyword validation)
;; -----------------------------------------------------------------------------

(def-op *qbe* :assign (var type op &rest args)
  (unless (member type *qbe-base-types* :test #'eq)
    (error "QBE: Invalid base type ~S in assignment. Expected :w, :l, :s, or :d." type))
  (unless (member op *qbe-assign-opcodes* :test #'eq)
    (error "QBE: Invalid assignment opcode ~S." op))
  
  (format nil "~a =~(~a~) ~(~a~) ~{~a~^, ~}"
          (lower var) type op (mapcar #'lower args)))

(def-op *qbe* :instr (op &rest args)
  (unless (member op *qbe-effect-opcodes* :test #'eq)
    (error "QBE: Invalid effectful opcode ~S." op))
  
  (format nil "~(~a~) ~{~a~^, ~}" op (mapcar #'lower args)))


;; -----------------------------------------------------------------------------
;; Declarations (Updated for keyword types)
;; -----------------------------------------------------------------------------

(def-op *qbe* :field (type &optional count)
  (unless (member type *qbe-ext-types* :test #'eq)
    (error "QBE: Invalid extended type ~S in field." type))
  (if count
      (format nil "~(~a~) ~a" type count)
      (format nil "~(~a~)" type)))

(def-op *qbe* :data-item (type &rest vals)
  (unless (member type *qbe-ext-types* :test #'eq)
    (error "QBE: Invalid extended type ~S in data-item." type))
  (if (eq type :z)
      (format nil "z ~a" (first vals))
      (format nil "~(~a~) ~{~a~^ ~}" type vals)))

(def-op *qbe* :param (type &optional name)
  (cond ((eq type :...) "...")
        ((eq type :env) (format nil "env ~a" (lower name)))
        (t (unless (member type *qbe-base-types* :test #'eq)
             (error "QBE: Invalid type ~S in param." type))
           (format nil "~(~a~) ~a" type (lower name)))))

(def-op *qbe* :call-assign (var type target &rest args)
  (unless (member type *qbe-base-types* :test #'eq)
    (error "QBE: Invalid return type ~S in call." type))
  (format nil "~a =~(~a~) call ~a(~{~a~^, ~})"
          (lower var) type (lower target) (mapcar #'lower args)))

(def-op *qbe* :call-arg (type val)
  (cond ((eq type :...) "...")
        ((eq type :env) (format nil "env ~a" (lower val)))
        (t (format nil "~(~a~) ~a" type (lower val)))))

(def-op *qbe* :call (target &rest args)
  (format nil "call ~a(~{~a~^, ~})"
          (lower target) (mapcar #'lower args)))

(def-op *qbe* :phi (var type &rest args)
  (let ((pairs (loop for (lbl val) on args by #'cddr
                     collect (format nil "~a ~a" (lower lbl) (lower val)))))
    (format nil "~a =~(~a~) phi ~{~a~^, ~}" (lower var) type pairs)))


;; -----------------------------------------------------------------------------
;; Building
;; -----------------------------------------------------------------------------
(defun build-qbe-ast (ast &key 
                            (out-name "program") 
                            (runtime-c "runtime.c") 
                            (keep-temp-files t))
  "Compiles a QBE AST into an executable using `qbe` and `clang`."
  (let ((ssa-file (format nil "~a.ssa" out-name))
        (asm-file (format nil "~a.s" out-name))
        ;; Lower the AST to a string using our interpreter
        (il-string (lower *qbe* ast)))
    
    ;; 1. Write the Intermediate Language to a .ssa file
    (format t ";; Writing IL to ~S~%" ssa-file)
    (with-open-file (stream ssa-file :direction :output :if-exists :supersede)
      (write-string il-string stream))
    
    ;; 2. Run QBE to compile .ssa to assembly (.s)
    (format t ";; Running QBE: qbe -o ~a ~a~%" asm-file ssa-file)
    (handler-case
        (uiop:run-program (list "qbe" "-o" asm-file ssa-file)
                          :output *standard-output*
                          :error-output *error-output*)
      (error (e)
        (error "QBE compilation failed: ~a" e)))
    
    ;; 3. Run Clang to compile the assembly, link with runtime.c, and output an executable
    (format t ";; Running Clang: clang -o ~a ~a ~a~%" out-name asm-file runtime-c)
    (handler-case
        (uiop:run-program (list "clang" "-o" out-name asm-file runtime-c)
                          :output *standard-output*
                          :error-output *error-output*)
      (error (e)
        (error "Clang linking failed: ~a" e)))
    
    ;; 4. Clean up intermediate files if requested
    (unless keep-temp-files
      (delete-file ssa-file)
      (delete-file asm-file)
      (format t ";; Cleaned up intermediate files.~%"))
    
    (format t ";; Build complete! Executable created: ./~a~%" out-name)
    
    ;; Return the path to the executable
    out-name))
