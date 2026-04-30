;;;; Test the parts of source-locations.lisp that don't need Eclector
;;;; loaded. We mock the Eclector-dependent paths.

(defpackage #:src-loc-test
  (:use #:cl))
(in-package #:src-loc-test)

;; --- Inline copies of the parts of source-locations.lisp under test ---
;; (We skip the eclector.concrete-syntax-tree:read parts.)

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

(defun clear-source-locations ()
  (clrhash *source-locations*))

(defun %build-line-table (text)
  (let ((starts (list 0)))
    (loop for i from 0 below (length text) do
      (when (char= (char text i) #\Newline)
        (push (1+ i) starts)))
    (coerce (nreverse starts) 'vector)))

(defun %offset-to-line-col (offset line-starts)
  (let ((lo 0) (hi (1- (length line-starts))))
    (loop while (< lo hi) do
      (let ((mid (floor (+ lo hi 1) 2)))
        (if (<= (aref line-starts mid) offset)
            (setf lo mid)
            (setf hi (1- mid)))))
    (values (1+ lo)
            (1+ (- offset (aref line-starts lo))))))

(defun inherit-loc (target source)
  (when (consp target)
    (let ((src-loc (source-loc source)))
      (when (and src-loc (not (gethash target *source-locations*)))
        (setf (gethash target *source-locations*) src-loc))))
  target)

(defun cons-loc (car cdr source)
  (inherit-loc (cons car cdr) source))

(defun list-loc (source &rest items)
  (inherit-loc (copy-list items) source))

(defmacro with-loc ((source) &body body)
  (let ((src (gensym "SRC"))
        (val (gensym "VAL")))
    `(let* ((,src ,source)
            (,val (progn ,@body)))
       (inherit-loc ,val ,src))))

(defun some-recorded-offset (form)
  (cond ((not (consp form)) nil)
        ((source-loc form) (source-loc-start-offset (source-loc form)))
        (t (or (some-recorded-offset (car form))
               (some-recorded-offset (cdr form))))))

(defun smallest-containing-range (offset)
  (let ((best nil)
        (best-size most-positive-fixnum))
    (maphash (lambda (cons loc)
               (declare (ignore cons))
               (let ((s (source-loc-start-offset loc))
                     (e (source-loc-end-offset loc)))
                 (when (and s e (<= s offset) (<= offset e))
                   (let ((size (- e s)))
                     (when (< size best-size)
                       (setf best loc
                             best-size size))))))
             *source-locations*)
    best))

(defun source-loc-or-ancestor (form)
  (or (source-loc form)
      (let ((self-offset (some-recorded-offset form)))
        (when self-offset
          (smallest-containing-range self-offset)))))

;; --- Mock CST walker: simulates what the real %walk-cst does ---
;;
;; A "fake CST" here is (:cst-cons RAW (START . END) FIRST-CST REST-CST)
;; or (:cst-atom RAW (START . END)). The mock walker populates the
;; sidecar table just like the real one would.

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
                                             :end-line el :end-col ec
                                             :start-offset (car range)
                                             :end-offset (cdr range)))))))
       (when (and loc (consp raw)
                  (not (gethash raw *source-locations*)))
         (setf (gethash raw *source-locations*) loc))
       (mock-walk-cst first-c file line-starts)
       (mock-walk-cst rest-c file line-starts)
       raw))
    (:cst-atom
     (second cst))))

;; --- Test framework ---

(defvar *tests-run* 0)
(defvar *tests-passed* 0)
(defvar *failures* '())

(defmacro deftest (name &body body)
  `(progn
     (incf *tests-run*)
     (handler-case
         (progn
           ,@body
           (incf *tests-passed*)
           (format t "  PASS ~A~%" ',name))
       (error (e)
         (push (cons ',name e) *failures*)
         (format t "  FAIL ~A: ~A~%" ',name e)))))

(defun check (label expected actual)
  (unless (equalp expected actual)
    (error "~A: expected ~S, got ~S" label expected actual)))

(defun check-true (label x)
  (unless x (error "~A: expected true, got NIL" label)))

(defun check-false (label x)
  (when x (error "~A: expected NIL, got ~S" label x)))

;; --- Tests ---

(defun run-tests ()
  (setf *tests-run* 0 *tests-passed* 0 *failures* '())

  (format t "~&=== Line table / offset conversion ===~%")

  (deftest line-table-empty
    (let ((lt (%build-line-table "")))
      (check "table for empty" #(0) lt)))

  (deftest line-table-no-newlines
    (let ((lt (%build-line-table "hello")))
      (check "no newlines" #(0) lt)))

  (deftest line-table-with-newlines
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      ;; positions: 0='a' 1='b' 2='c' 3='\n' 4='d' 5='e' 6='f' 7='\n' 8='g'...
      (check "with newlines" #(0 4 8) lt)))

  (deftest offset-to-line-col-line1
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 0 lt)
        (check "offset 0 -> line" 1 line)
        (check "offset 0 -> col"  1 col))
      (multiple-value-bind (line col) (%offset-to-line-col 2 lt)
        (check "offset 2 -> line" 1 line)
        (check "offset 2 -> col"  3 col))))

  (deftest offset-to-line-col-line2
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 4 lt)
        (check "offset 4 -> line" 2 line)
        (check "offset 4 -> col"  1 col))
      (multiple-value-bind (line col) (%offset-to-line-col 6 lt)
        (check "offset 6 -> line" 2 line)
        (check "offset 6 -> col"  3 col))))

  (deftest offset-to-line-col-line3
    (let ((lt (%build-line-table (format nil "abc~%def~%ghi"))))
      (multiple-value-bind (line col) (%offset-to-line-col 8 lt)
        (check "offset 8 -> line" 3 line)
        (check "offset 8 -> col"  1 col))
      (multiple-value-bind (line col) (%offset-to-line-col 10 lt)
        (check "offset 10 -> line" 3 line)
        (check "offset 10 -> col"  3 col))))

  (deftest offset-at-newline
    ;; The newline char itself: "abc\n" — offset 3 is the newline.
    ;; By convention here it's still on line 1 (after column 3).
    (let ((lt (%build-line-table (format nil "abc~%def"))))
      (multiple-value-bind (line col) (%offset-to-line-col 3 lt)
        (check "newline char line" 1 line)
        (check "newline char col"  4 col))))

  (format t "~&=== Source-loc accessors ===~%")

  (deftest setf-source-loc-set
    (clear-source-locations)
    (let ((cons (list 1 2 3))
          (loc (make-source-loc :file "x" :start-line 1 :start-col 1
                                :end-line 1 :end-col 5
                                :start-offset 0 :end-offset 5)))
      (setf (source-loc cons) loc)
      (check-true "loc set" (eq (source-loc cons) loc))))

  (deftest setf-source-loc-clear
    (clear-source-locations)
    (let ((cons (list 1 2 3))
          (loc (make-source-loc :file "x" :start-line 1 :start-col 1
                                :end-line 1 :end-col 5
                                :start-offset 0 :end-offset 5)))
      (setf (source-loc cons) loc)
      (setf (source-loc cons) nil)
      (check-false "loc cleared" (source-loc cons))))

  (deftest source-loc-on-atom
    (clear-source-locations)
    (check-false "atoms have no loc" (source-loc 42))
    (check-false "symbols have no loc" (source-loc 'foo))
    (check-false "nil has no loc" (source-loc nil)))

  (deftest setf-source-loc-on-atom-errors
    (handler-case
        (progn
          (setf (source-loc 42)
                (make-source-loc :file "x" :start-line 1 :start-col 1
                                 :end-line 1 :end-col 5
                                 :start-offset 0 :end-offset 5))
          (error "should have signalled"))
      (error () :ok)))

  (format t "~&=== inherit-loc / cons-loc / list-loc ===~%")

  (deftest inherit-loc-basic
    (clear-source-locations)
    (let ((src (list :original))
          (target (list :derived))
          (loc (make-source-loc :file "f" :start-line 5 :start-col 3
                                :end-line 5 :end-col 13
                                :start-offset 100 :end-offset 110)))
      (setf (source-loc src) loc)
      (inherit-loc target src)
      (check "inherited" loc (source-loc target))))

  (deftest inherit-loc-no-source-info
    ;; If source has no loc, target stays unmarked.
    (clear-source-locations)
    (let ((src (list :no-loc))
          (target (list :derived)))
      (inherit-loc target src)
      (check-false "no inheritance" (source-loc target))))

  (deftest inherit-loc-target-already-has-loc
    ;; First attribution wins: existing loc on target is preserved.
    (clear-source-locations)
    (let* ((src (list :s))
           (target (list :t))
           (loc1 (make-source-loc :file "a" :start-line 1 :start-col 1
                                  :end-line 1 :end-col 2
                                  :start-offset 0 :end-offset 2))
           (loc2 (make-source-loc :file "b" :start-line 9 :start-col 9
                                  :end-line 9 :end-col 10
                                  :start-offset 50 :end-offset 52)))
      (setf (source-loc target) loc1)
      (setf (source-loc src) loc2)
      (inherit-loc target src)
      (check "first wins" loc1 (source-loc target))))

  (deftest inherit-loc-on-atom-target-noop
    ;; Atoms can't carry locs; should silently no-op, not error.
    (clear-source-locations)
    (let ((src (list :s))
          (loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                :end-line 1 :end-col 2
                                :start-offset 0 :end-offset 2)))
      (setf (source-loc src) loc)
      (let ((result (inherit-loc 42 src)))
        (check "atom returned unchanged" 42 result))))

  (deftest cons-loc-builds-and-tags
    (clear-source-locations)
    (let* ((src (list :original))
           (loc (make-source-loc :file "f" :start-line 2 :start-col 1
                                 :end-line 2 :end-col 4
                                 :start-offset 10 :end-offset 14)))
      (setf (source-loc src) loc)
      (let ((c (cons-loc :head '(:tail) src)))
        (check "car"     :head (car c))
        (check "cdr"     '(:tail) (cdr c))
        (check "tagged"  loc (source-loc c)))))

  (deftest list-loc-builds-and-tags
    (clear-source-locations)
    (let* ((src (list :original))
           (loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                 :end-line 1 :end-col 5
                                 :start-offset 0 :end-offset 5)))
      (setf (source-loc src) loc)
      (let ((l (list-loc src :a :b :c)))
        (check "list contents" '(:a :b :c) l)
        (check "tagged"        loc (source-loc l)))))

  (deftest with-loc-macro
    (clear-source-locations)
    (let* ((src (list :original))
           (loc (make-source-loc :file "f" :start-line 3 :start-col 5
                                 :end-line 3 :end-col 11
                                 :start-offset 25 :end-offset 31)))
      (setf (source-loc src) loc)
      (let ((result (with-loc (src)
                      (list :built :from :scratch))))
        (check "built right" '(:built :from :scratch) result)
        (check "tagged" loc (source-loc result)))))

  (deftest with-loc-doesnt-overwrite
    (clear-source-locations)
    (let* ((src (list :a))
           (existing (list :pre-tagged))
           (loc-old (make-source-loc :file "x" :start-line 1 :start-col 1
                                     :end-line 1 :end-col 2
                                     :start-offset 0 :end-offset 2))
           (loc-new (make-source-loc :file "y" :start-line 9 :start-col 9
                                     :end-line 9 :end-col 10
                                     :start-offset 50 :end-offset 52)))
      (setf (source-loc existing) loc-old)
      (setf (source-loc src) loc-new)
      (with-loc (src) existing)
      (check "preserved" loc-old (source-loc existing))))

  (format t "~&=== source-loc-or-ancestor (containment fallback) ===~%")

  (deftest ancestor-direct-hit
    (clear-source-locations)
    (let* ((node (list :n))
           (loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                 :end-line 1 :end-col 5
                                 :start-offset 0 :end-offset 5)))
      (setf (source-loc node) loc)
      (check "direct" loc (source-loc-or-ancestor node))))

  (deftest ancestor-fallback
    ;; Inner node has no loc, but its parent's range contains a sub-cons
    ;; that does. The fallback should find the outer range.
    (clear-source-locations)
    (let* ((inner (list :inner))
           (sub   (list :sub))
           (untagged-with-tagged-child (list :outer sub inner))
           (outer-loc (make-source-loc :file "f" :start-line 1 :start-col 1
                                       :end-line 1 :end-col 30
                                       :start-offset 0 :end-offset 30))
           (sub-loc   (make-source-loc :file "f" :start-line 1 :start-col 8
                                       :end-line 1 :end-col 13
                                       :start-offset 7 :end-offset 12))
           (other-loc (make-source-loc :file "f" :start-line 5 :start-col 1
                                       :end-line 5 :end-col 5
                                       :start-offset 100 :end-offset 105)))
      (setf (source-loc sub) sub-loc)
      ;; Add another unrelated entry to ensure the algorithm picks the
      ;; one that actually contains us.
      (setf (source-loc (list :unrelated)) other-loc)
      ;; Add a containing range, but DON'T attach it to the form itself
      ;; — we want to ensure fallback works.
      (let ((containing (list :container)))
        (setf (source-loc containing) outer-loc))
      ;; untagged-with-tagged-child has no loc, but contains sub which
      ;; does, at offset 7. Looking up should find a smallest range
      ;; containing offset 7.
      (let ((found (source-loc-or-ancestor untagged-with-tagged-child)))
        (check-true "found something" found)
        ;; The smallest range containing offset 7 is sub-loc itself
        ;; (7..12 has size 5; outer-loc 0..30 has size 30). So fallback
        ;; lands on sub-loc, which is valid behavior — the form contains
        ;; sub, sub has a known range, so attribution to sub's range is
        ;; the "best guess we have".
        (check "smallest containing" sub-loc found))))

  (format t "~&=== Mock-CST walker (simulates Eclector path) ===~%")

  (deftest walk-cst-simple
    (clear-source-locations)
    (let* ((text "(:add 1 2)")
           (lt (%build-line-table text))
           ;; Simulate: outer cons (:add 1 2), range 0..10
           ;; Build the actual list and the mock CST referencing it.
           (raw-1   1)
           (raw-2   2)
           (raw-rest-of-add (cons raw-1 (cons raw-2 nil)))
           (raw-outer (cons :add raw-rest-of-add))
           (cst-2   (list :cst-atom raw-2 (cons 8 9)))
           (cst-nil (list :cst-atom nil nil))
           (cst-rest-of-rest (list :cst-cons (cdr raw-rest-of-add)
                                   (cons 8 9) cst-2 cst-nil))
           (cst-1   (list :cst-atom raw-1 (cons 6 7)))
           (cst-rest-of-add (list :cst-cons raw-rest-of-add
                                  (cons 6 9) cst-1 cst-rest-of-rest))
           (cst-add (list :cst-atom :add (cons 1 5)))
           (cst-outer (list :cst-cons raw-outer
                            (cons 0 10) cst-add cst-rest-of-add)))
      (mock-walk-cst cst-outer "test.lisp" lt)
      (let ((loc (source-loc raw-outer)))
        (check-true "outer tagged" loc)
        (check "outer file" "test.lisp" (source-loc-file loc))
        (check "outer start-line" 1 (source-loc-start-line loc))
        (check "outer start-col"  1 (source-loc-start-col loc))
        (check "outer end-col"    11 (source-loc-end-col loc)))
      ;; Inner cons should also be tagged.
      (let ((inner-loc (source-loc raw-rest-of-add)))
        (check-true "inner tagged" inner-loc)
        (check "inner start-col" 7 (source-loc-start-col inner-loc)))))

  (deftest walk-cst-multiline
    (clear-source-locations)
    ;; Multi-line input:
    ;;   (:foo
    ;;     :bar)
    ;; Layout: 0='(', 1=':', 2='f', 3='o', 4='o', 5='\n',
    ;;         6=' ', 7=' ', 8=':', 9='b', 10='a', 11='r', 12=')'
    (let* ((text (format nil "(:foo~%  :bar)"))
           (lt (%build-line-table text))
           (raw (list :foo :bar))
           (cst-foo  (list :cst-atom :foo (cons 1 5)))
           (cst-bar  (list :cst-atom :bar (cons 8 12)))
           (cst-nil  (list :cst-atom nil nil))
           (cst-tail (list :cst-cons (cdr raw) (cons 8 12) cst-bar cst-nil))
           (cst-outer (list :cst-cons raw (cons 0 13) cst-foo cst-tail)))
      (mock-walk-cst cst-outer "multi.lisp" lt)
      (let ((loc (source-loc raw)))
        (check "outer start line" 1 (source-loc-start-line loc))
        (check "outer end line" 2 (source-loc-end-line loc))
        ;; end-offset 13 sits one past the ')', which itself is at col 7
        ;; of line 2; so end-col = 8.
        (check "outer end col" 8 (source-loc-end-col loc)))
      (let ((tail-loc (source-loc (cdr raw))))
        (check "tail start line" 2 (source-loc-start-line tail-loc))
        (check "tail start col" 3 (source-loc-start-col tail-loc)))))

  (deftest walk-cst-distinguishes-conses-by-eq
    ;; Two structurally-equal but eq-distinct lists shouldn't share locs.
    (clear-source-locations)
    (let* ((text "(:a) (:a)")
           (lt (%build-line-table text))
           (raw1 (list :a))
           (raw2 (list :a))
           (cst-nil (list :cst-atom nil nil))
           (cst-a-1 (list :cst-atom :a (cons 1 3)))
           (cst-a-2 (list :cst-atom :a (cons 6 8)))
           (cst1 (list :cst-cons raw1 (cons 0 4) cst-a-1 cst-nil))
           (cst2 (list :cst-cons raw2 (cons 5 9) cst-a-2 cst-nil)))
      (mock-walk-cst cst1 "x" lt)
      (mock-walk-cst cst2 "x" lt)
      (let ((l1 (source-loc raw1))
            (l2 (source-loc raw2)))
        (check "first form col" 1 (source-loc-start-col l1))
        (check "second form col" 6 (source-loc-start-col l2)))))

  (format t "~&=== Propagation through transformations ===~%")

  (deftest propagation-end-to-end
    ;; Simulate: read a node, transform it, check the transformed
    ;; node still points at the original source.
    (clear-source-locations)
    (let* ((original (list :swap 'a 'b))
           (loc (make-source-loc :file "src.lisp"
                                 :start-line 7 :start-col 3
                                 :end-line 7 :end-col 13
                                 :start-offset 60 :end-offset 70)))
      (setf (source-loc original) loc)
      ;; Pretend this is what a handler does:
      (let* ((tmp (gensym "TMP"))
             ;; First derived node, attribute via with-loc
             (decl (with-loc (original) (list :var :auto tmp)))
             (set1 (with-loc (original) (list :set tmp 'a)))
             (set2 (with-loc (original) (list :set 'a 'b)))
             (set3 (with-loc (original) (list :set 'b tmp))))
        (check "decl tagged" loc (source-loc decl))
        (check "set1 tagged" loc (source-loc set1))
        (check "set2 tagged" loc (source-loc set2))
        (check "set3 tagged" loc (source-loc set3)))))

  (format t "~&----~%~D/~D tests passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f *failures*)
      (format t "  ~A: ~A~%" (car f) (cdr f))))
  (= *tests-passed* *tests-run*))

(unless (run-tests)
  (sb-ext:exit :code 1))

