(in-package #:tagless-compiler)

;;; Tests for the source-location machinery in source.lisp.
;;; Uses a mock CST walker (mock-walk-cst) to simulate what the real
;;; %walk-cst / Eclector path does, allowing testing without loading Eclector.

;;; --- Mock CST walker ---
;;;
;;; A "fake CST" node is one of:
;;;   (:cst-cons RAW (START . END) FIRST-CST REST-CST)
;;;   (:cst-atom RAW (START . END))
;;; The walker populates *source-locations* just like the real %walk-cst.

(defun mock-walk-cst (cst file line-starts)
  (case (first cst)
    (:cst-cons
     (let* ((raw      (second cst))
            (range    (third cst))
            (first-c  (fourth cst))
            (rest-c   (fifth cst))
            (loc      (when range
                        (multiple-value-bind (sl sc)
                            (%offset-to-line-col (car range) line-starts)
                          (multiple-value-bind (el ec)
                              (%offset-to-line-col (cdr range) line-starts)
                            (make-source-loc :file file
                                             :start-line sl :start-col sc
                                             :end-line   el :end-col   ec
                                             :start-offset (car range)
                                             :end-offset   (cdr range)))))))
       (when (and loc (consp raw)
                  (not (gethash raw *source-locations*)))
         (setf (gethash raw *source-locations*) loc))
       (mock-walk-cst first-c file line-starts)
       (mock-walk-cst rest-c  file line-starts)
       raw))
    (:cst-atom
     (second cst))))

;;; ---------------------------------------------------------------------------
;;; Suite: line table and offset conversion
;;; ---------------------------------------------------------------------------

(defsuite "source-loc: line table"
  (deftest "empty string"
    (check (%build-line-table "") #(0) :test #'equalp))

  (deftest "no newlines"
    (check (%build-line-table "hello") #(0) :test #'equalp))

  (deftest "with newlines"
    ;; "abc\ndef\nghi" → line starts at 0, 4, 8
    (check (%build-line-table (format nil "abc~%def~%ghi")) #(0 4 8) :test #'equalp))

  (deftest "offset 0 → line 1 col 1"
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 0 lt)
        (check line 1)
        (check col  1))))

  (deftest "offset 2 → line 1 col 3"
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 2 lt)
        (check line 1)
        (check col  3))))

  (deftest "offset 4 → line 2 col 1"
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 4 lt)
        (check line 2)
        (check col  1))))

  (deftest "offset 6 → line 2 col 3"
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 6 lt)
        (check line 2)
        (check col  3))))

  (deftest "offset 8 → line 3 col 1"
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 8 lt)
        (check line 3)
        (check col  1))))

  (deftest "offset 10 → line 3 col 3"
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 10 lt)
        (check line 3)
        (check col  3))))

  (deftest "newline char itself is still on prev line"
    ;; "abc\ndef" — offset 3 is the newline; still line 1
    (let ((lt (%build-line-table (format nil "abc~%def"))))
      (multiple-value-bind (line col) (%offset-to-line-col 3 lt)
        (check line 1)
        (check col  4)))))

;;; ---------------------------------------------------------------------------
;;; Suite: source-loc accessors
;;; ---------------------------------------------------------------------------

(defsuite "source-loc: accessors"
  (deftest "setf source-loc stores a loc"
    (clear-source-locations)
    (let ((cons (list 1 2 3))
          (loc  (make-source-loc :file "x" :start-line 1 :start-col 1
                                 :end-line 1 :end-col 5
                                 :start-offset 0 :end-offset 5)))
      (setf (source-loc cons) loc)
      (check-true (eq (source-loc cons) loc))))

  (deftest "setf source-loc nil clears the loc"
    (clear-source-locations)
    (let ((cons (list 1 2 3))
          (loc  (make-source-loc :file "x" :start-line 1 :start-col 1
                                 :end-line 1 :end-col 5
                                 :start-offset 0 :end-offset 5)))
      (setf (source-loc cons) loc)
      (setf (source-loc cons) nil)
      (check-false (source-loc cons))))

  (deftest "atoms always return nil loc"
    (clear-source-locations)
    (check-false (source-loc 42))
    (check-false (source-loc 'foo))
    (check-false (source-loc nil)))

  (deftest "setf source-loc on atom signals"
    (check-error
      (setf (source-loc 42)
            (make-source-loc :file "x" :start-line 1 :start-col 1
                             :end-line 1 :end-col 5
                             :start-offset 0 :end-offset 5)))))

;;; ---------------------------------------------------------------------------
;;; Suite: inherit-loc / cons-loc / list-loc / with-loc
;;; ---------------------------------------------------------------------------

(defsuite "source-loc: propagating constructors"
  (deftest "inherit-loc copies source loc onto unloc'd target"
    (clear-source-locations)
    (let ((src    (list :original))
          (target (list :derived))
          (loc    (make-source-loc :file "f" :start-line 5 :start-col 3
                                   :end-line 5 :end-col 13
                                   :start-offset 100 :end-offset 110)))
      (setf (source-loc src) loc)
      (inherit-loc target src)
      (check (source-loc target) loc)))

  (deftest "inherit-loc no-ops when source has no loc"
    (clear-source-locations)
    (let ((src    (list :no-loc))
          (target (list :derived)))
      (inherit-loc target src)
      (check-false (source-loc target))))

  (deftest "inherit-loc first-wins: existing loc preserved"
    (clear-source-locations)
    (let* ((src    (list :s))
           (target (list :t))
           (loc1   (make-source-loc :file "a" :start-line 1 :start-col 1
                                    :end-line 1 :end-col 2
                                    :start-offset 0 :end-offset 2))
           (loc2   (make-source-loc :file "b" :start-line 9 :start-col 9
                                    :end-line 9 :end-col 10
                                    :start-offset 50 :end-offset 52)))
      (setf (source-loc target) loc1)
      (setf (source-loc src) loc2)
      (inherit-loc target src)
      (check (source-loc target) loc1)))

  (deftest "inherit-loc on atom target is a no-op"
    (clear-source-locations)
    (let ((src (list :s))
          (loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                :end-line 1 :end-col 2
                                :start-offset 0 :end-offset 2)))
      (setf (source-loc src) loc)
      (check (inherit-loc 42 src) 42)))

  (deftest "cons-loc builds a tagged cons"
    (clear-source-locations)
    (let* ((src (list :original))
           (loc (make-source-loc :file "f" :start-line 2 :start-col 1
                                 :end-line 2 :end-col 4
                                 :start-offset 10 :end-offset 14)))
      (setf (source-loc src) loc)
      (let ((c (cons-loc :head '(:tail) src)))
        (check (car c) :head)
        (check (cdr c) '(:tail))
        (check (source-loc c) loc))))

  (deftest "list-loc builds a tagged list"
    (clear-source-locations)
    (let* ((src (list :original))
           (loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                 :end-line 1 :end-col 5
                                 :start-offset 0 :end-offset 5)))
      (setf (source-loc src) loc)
      (let ((l (list-loc src :a :b :c)))
        (check l '(:a :b :c))
        (check (source-loc l) loc))))

  (deftest "with-loc attributes result to source"
    (clear-source-locations)
    (let* ((src (list :original))
           (loc (make-source-loc :file "f" :start-line 3 :start-col 5
                                 :end-line 3 :end-col 11
                                 :start-offset 25 :end-offset 31)))
      (setf (source-loc src) loc)
      (let ((result (with-loc (src) (list :built :from :scratch))))
        (check result '(:built :from :scratch))
        (check (source-loc result) loc))))

  (deftest "with-loc does not overwrite existing loc"
    (clear-source-locations)
    (let* ((src      (list :a))
           (existing (list :pre-tagged))
           (loc-old  (make-source-loc :file "x" :start-line 1 :start-col 1
                                      :end-line 1 :end-col 2
                                      :start-offset 0 :end-offset 2))
           (loc-new  (make-source-loc :file "y" :start-line 9 :start-col 9
                                      :end-line 9 :end-col 10
                                      :start-offset 50 :end-offset 52)))
      (setf (source-loc existing) loc-old)
      (setf (source-loc src) loc-new)
      (with-loc (src) existing)
      (check (source-loc existing) loc-old))))

;;; ---------------------------------------------------------------------------
;;; Suite: source-loc-or-ancestor
;;; ---------------------------------------------------------------------------

(defsuite "source-loc: ancestor fallback"
  (deftest "direct hit"
    (clear-source-locations)
    (let* ((node (list :n))
           (loc  (make-source-loc :file "f" :start-line 1 :start-col 1
                                  :end-line 1 :end-col 5
                                  :start-offset 0 :end-offset 5)))
      (setf (source-loc node) loc)
      (check (source-loc-or-ancestor node) loc)))

  (deftest "fallback to smallest containing range"
    (clear-source-locations)
    (let* ((inner  (list :inner))
           (sub    (list :sub))
           (form   (list :outer sub inner))
           (sub-loc (make-source-loc :file "f" :start-line 1 :start-col 8
                                     :end-line 1 :end-col 13
                                     :start-offset 7 :end-offset 12))
           (outer-loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                       :end-line 1 :end-col 30
                                       :start-offset 0 :end-offset 30)))
      (setf (source-loc sub) sub-loc)
      ;; Add a containing range that is NOT directly attached to `form`.
      (let ((container (list :container)))
        (setf (source-loc container) outer-loc))
      ;; `form` has no direct loc, but `sub` inside it does (offset 7).
      ;; The smallest range containing offset 7 is sub-loc (size 5 < 30).
      (let ((found (source-loc-or-ancestor form)))
        (check-true found)
        (check found sub-loc)))))

;;; ---------------------------------------------------------------------------
;;; Suite: mock CST walker
;;; ---------------------------------------------------------------------------

(defsuite "source-loc: mock CST walker"
  (deftest "single-line form gets correct col range"
    (clear-source-locations)
    (let* ((text "(:add 1 2)")
           (lt   (%build-line-table text))
           (raw-1 1)
           (raw-2 2)
           (raw-rest (cons raw-1 (cons raw-2 nil)))
           (raw-outer (cons :add raw-rest))
           (cst-nil      (list :cst-atom nil nil))
           (cst-2        (list :cst-atom raw-2 (cons 8 9)))
           (cst-rest-rest (list :cst-cons (cdr raw-rest) (cons 8 9) cst-2 cst-nil))
           (cst-1        (list :cst-atom raw-1 (cons 6 7)))
           (cst-rest     (list :cst-cons raw-rest (cons 6 9) cst-1 cst-rest-rest))
           (cst-add      (list :cst-atom :add (cons 1 5)))
           (cst-outer    (list :cst-cons raw-outer (cons 0 10) cst-add cst-rest)))
      (mock-walk-cst cst-outer "test.lisp" lt)
      (let ((loc (source-loc raw-outer)))
        (check-true loc)
        (check (source-loc-file loc) "test.lisp")
        (check (source-loc-start-line loc) 1)
        (check (source-loc-start-col loc)  1)
        (check (source-loc-end-col loc)    11))
      (let ((inner-loc (source-loc raw-rest)))
        (check-true inner-loc)
        (check (source-loc-start-col inner-loc) 7))))

  (deftest "multi-line form gets correct line range"
    (clear-source-locations)
    ;; "(:foo\n  :bar)"
    (let* ((text (format nil "(:foo~%  :bar)"))
           (lt   (%build-line-table text))
           (raw  (list :foo :bar))
           (cst-foo  (list :cst-atom :foo (cons 1 5)))
           (cst-bar  (list :cst-atom :bar (cons 8 12)))
           (cst-nil  (list :cst-atom nil nil))
           (cst-tail (list :cst-cons (cdr raw) (cons 8 12) cst-bar cst-nil))
           (cst-outer (list :cst-cons raw (cons 0 13) cst-foo cst-tail)))
      (mock-walk-cst cst-outer "multi.lisp" lt)
      (let ((loc (source-loc raw)))
        (check (source-loc-start-line loc) 1)
        (check (source-loc-end-line loc)   2)
        (check (source-loc-end-col loc)    8))
      (let ((tail-loc (source-loc (cdr raw))))
        (check (source-loc-start-line tail-loc) 2)
        (check (source-loc-start-col tail-loc)  3))))

  (deftest "eq-distinct conses get distinct locs"
    (clear-source-locations)
    (let* ((text "(:a) (:a)")
           (lt   (%build-line-table text))
           (raw1 (list :a))
           (raw2 (list :a))
           (cst-nil  (list :cst-atom nil nil))
           (cst-a-1  (list :cst-atom :a (cons 1 3)))
           (cst-a-2  (list :cst-atom :a (cons 6 8)))
           (cst1 (list :cst-cons raw1 (cons 0 4) cst-a-1 cst-nil))
           (cst2 (list :cst-cons raw2 (cons 5 9) cst-a-2 cst-nil)))
      (mock-walk-cst cst1 "x" lt)
      (mock-walk-cst cst2 "x" lt)
      (check (source-loc-start-col (source-loc raw1)) 1)
      (check (source-loc-start-col (source-loc raw2)) 6)))

  (deftest "propagation end-to-end"
    ;; Tag a node, transform it (simulating a handler), check derived nodes.
    (clear-source-locations)
    (let* ((original (list :swap 'a 'b))
           (loc      (make-source-loc :file "src.lisp"
                                      :start-line 7 :start-col 3
                                      :end-line 7 :end-col 13
                                      :start-offset 60 :end-offset 70)))
      (setf (source-loc original) loc)
      (let* ((tmp  (gensym "TMP"))
             (decl (with-loc (original) (list :var :auto tmp)))
             (set1 (with-loc (original) (list :set tmp 'a)))
             (set2 (with-loc (original) (list :set 'a 'b)))
             (set3 (with-loc (original) (list :set 'b tmp))))
        (check (source-loc decl) loc)
        (check (source-loc set1) loc)
        (check (source-loc set2) loc)
        (check (source-loc set3) loc)))))
