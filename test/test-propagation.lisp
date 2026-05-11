(in-package #:tagless-compiler)

;;; End-to-end tests for source-loc propagation through interpreter passes.
;;; Uses the real make-interpreter / lower / inherit-loc from interpreter.lisp
;;; and source.lisp.  Handlers are registered directly into the interpreter's
;;; hash table (without def-op) so tests stay self-contained.

(defun tag-form (form file line)
  "Attach a synthetic source-loc to FORM and return it."
  (setf (source-loc form)
        (make-source-loc :file file :start-line line :start-col 1
                         :end-line line :end-col 10
                         :start-offset (* line 100)
                         :end-offset   (+ (* line 100) 10)))
  form)

;;; ---------------------------------------------------------------------------
;;; Suite: auto-propagation basics
;;; ---------------------------------------------------------------------------

(defsuite "source-loc propagation: basics"
  (deftest "passthrough preserves loc"
    (clear-source-locations)
    (let* ((interp (make-interpreter :on-unknown :passthrough))
           (input  (tag-form (list :unknown 1 2) "f.lisp" 5)))
      (let ((result (lower interp input)))
        (check-true (eq result input))
        (check-true (source-loc result)))))

  (deftest "fresh cons from handler inherits input loc"
    (clear-source-locations)
    (let* ((interp (make-interpreter)))
      (setf (gethash :double (handlers interp))
            (lambda (expr)
              (declare (ignore expr))
              (list :pair :hello :world)))
      (let* ((input  (tag-form (list :double) "f.lisp" 7))
             (result (lower interp input)))
        (check-true (consp result))
        (check-false (eq result input))
        (let ((loc (source-loc result)))
          (check-true loc)
          (check (source-loc-start-line loc) 7)))))

  (deftest "atom result from handler does not error"
    (clear-source-locations)
    (let ((interp (make-interpreter)))
      (setf (gethash :extract-atom (handlers interp))
            (lambda (expr) (declare (ignore expr)) 42))
      (let* ((input  (tag-form (list :extract-atom) "f.lisp" 3))
             (result (lower interp input)))
        (check result 42)))))

;;; ---------------------------------------------------------------------------
;;; Suite: splice context
;;; ---------------------------------------------------------------------------

(defsuite "source-loc propagation: splice"
  (deftest "each spliced node inherits from input"
    (clear-source-locations)
    (let ((interp (make-interpreter :on-unknown :passthrough)))
      (setf (gethash :triple (handlers interp))
            (lambda (expr)
              (declare (ignore expr))
              (splice (list :a) (list :b) (list :c))))
      (setf (gethash :block (handlers interp))
            (lambda (expr)
              (cons :block (mapcan (lambda (x) (lower interp x :splice t))
                                   (rest expr)))))
      (let* ((triple     (tag-form (list :triple) "f.lisp" 4))
             (block-form (tag-form (list :block triple) "f.lisp" 4))
             (result     (lower interp block-form)))
        (check result '(:block (:a) (:b) (:c)))
        (let ((loc-a (source-loc (second result)))
              (loc-b (source-loc (third  result)))
              (loc-c (source-loc (fourth result))))
          (check-true loc-a)
          (check-true loc-b)
          (check-true loc-c)
          (check (source-loc-start-line loc-a) 4)))))

  (deftest "empty splice is valid"
    (clear-source-locations)
    (let ((interp (make-interpreter :on-unknown :passthrough)))
      (setf (gethash :drop (handlers interp))
            (lambda (expr) (declare (ignore expr)) (splice)))
      (setf (gethash :block (handlers interp))
            (lambda (expr)
              (cons :block (mapcan (lambda (x) (lower interp x :splice t))
                                   (rest expr)))))
      (let* ((drop       (tag-form (list :drop) "f.lisp" 1))
             (block-form (tag-form (list :block drop) "f.lisp" 1))
             (result     (lower interp block-form)))
        (check result '(:block))))))

;;; ---------------------------------------------------------------------------
;;; Suite: first-wins and explicit tagging
;;; ---------------------------------------------------------------------------

(defsuite "source-loc propagation: first-wins"
  (deftest "handler-set loc beats auto-propagation"
    (clear-source-locations)
    (let ((interp (make-interpreter)))
      (setf (gethash :sub-from (handlers interp))
            (lambda (expr)
              (let ((sub (second expr))
                    (out (list :result :v)))
                (inherit-loc out sub)
                out)))
      (let* ((sub-form (tag-form (list :inner) "inner.lisp" 99))
             (input    (tag-form (list :sub-from sub-form) "outer.lisp" 5))
             (result   (lower interp input)))
        (let ((loc (source-loc result)))
          (check-true loc)
          (check (source-loc-start-line loc) 99)
          (check (source-loc-file loc) "inner.lisp"))))))

;;; ---------------------------------------------------------------------------
;;; Suite: on-unknown :recurse
;;; ---------------------------------------------------------------------------

(defsuite "source-loc propagation: recurse-unknown"
  (deftest "recurse rebuilds cons and tags it"
    (clear-source-locations)
    (let ((interp (make-interpreter :on-unknown :recurse)))
      (setf (gethash :double (handlers interp))
            (lambda (expr)
              (let ((x (second expr)))
                (list :pair x x))))
      (let* ((inner  (tag-form (list :double 7) "f.lisp" 10))
             (outer  (tag-form (list :wrap inner) "f.lisp" 11))
             (result (lower interp outer)))
        (check result '(:wrap (:pair 7 7)))
        (let ((outer-loc (source-loc result))
              (inner-loc (source-loc (second result))))
          (check-true outer-loc)
          (check (source-loc-start-line outer-loc) 11)
          (check-true inner-loc)
          (check (source-loc-start-line inner-loc) 10))))))

;;; ---------------------------------------------------------------------------
;;; Suite: multi-pass propagation
;;; ---------------------------------------------------------------------------

(defsuite "source-loc propagation: multi-pass"
  (deftest "loc survives two passes"
    (clear-source-locations)
    (let ((pass1 (make-interpreter :on-unknown :recurse))
          (pass2 (make-interpreter :on-unknown :recurse)))
      (setf (gethash :square (handlers pass1))
            (lambda (expr) (let ((x (second expr))) (list :mul x x))))
      (setf (gethash :mul (handlers pass2))
            (lambda (expr)
              (list :builtin-multiply (second expr) (third expr))))
      (let* ((sq      (tag-form (list :square 5) "src.lisp" 42))
             (after-1 (lower pass1 sq))
             (after-2 (lower pass2 after-1)))
        (check after-2 '(:builtin-multiply 5 5))
        (let ((loc (source-loc after-2)))
          (check-true loc)
          (check (source-loc-start-line loc) 42)
          (check (source-loc-file loc) "src.lisp")))))

  (deftest "disabling propagation suppresses auto-tagging"
    (clear-source-locations)
    (let ((interp (make-interpreter :propagate-source-locations nil)))
      (setf (gethash :ident (handlers interp))
            (lambda (expr) (list :result (second expr))))
      (let* ((input  (tag-form (list :ident 'x) "f.lisp" 1))
             (result (lower interp input)))
        (check-false (source-loc result))))))
