(in-package #:tagless-compiler)

;;; Tests for the source-location machinery in source.lisp.
;;; Uses a mock CST walker (mock-walk-cst) to simulate what the real
;;; %walk-cst / Eclector path does, allowing testing without loading Eclector.

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

