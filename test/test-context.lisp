;;;; Test the context snippet printer.
;;;;
;;;; We mock source-loc + the text cache to exercise print-source-context
;;;; without needing Eclector.

(defpackage #:ctx-test (:use #:cl))
(in-package #:ctx-test)

;; --- Source-loc support ---

(defstruct source-loc
  file start-line start-col end-line end-col start-offset end-offset)

(defvar *source-locations* (make-hash-table :test #'eq))
(defun source-loc (form) (and (consp form) (gethash form *source-locations*)))
(defun (setf source-loc) (loc form)
  (unless (consp form) (error "atom: ~S" form))
  (setf (gethash form *source-locations*) loc) loc)
(defun clear-locs () (clrhash *source-locations*))

(defun some-recorded-offset (form)
  (cond ((not (consp form)) nil)
        ((source-loc form) (source-loc-start-offset (source-loc form)))
        (t (or (some-recorded-offset (car form))
               (some-recorded-offset (cdr form))))))
(defun smallest-containing-range (offset)
  (let ((best nil) (best-size most-positive-fixnum))
    (maphash (lambda (c loc) (declare (ignore c))
               (let ((s (source-loc-start-offset loc))
                     (e (source-loc-end-offset loc)))
                 (when (and s e (<= s offset) (<= offset e))
                   (let ((sz (- e s)))
                     (when (< sz best-size) (setf best loc best-size sz))))))
             *source-locations*)
    best))
(defun source-loc-or-ancestor (form)
  (or (source-loc form)
      (let ((o (some-recorded-offset form)))
        (when o (smallest-containing-range o)))))

;; --- Source text cache ---

(defvar *source-texts* (make-hash-table :test #'equal))
(defun register-source-text (key text) (setf (gethash key *source-texts*) text))
(defun find-source-text (key) (gethash key *source-texts*))
(defun clear-texts () (clrhash *source-texts*))

;; --- The function under test ---

(defun %split-lines (text)
  (let ((lines '()) (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (subseq text start i) lines) (setf start (1+ i)))
    (when (<= start (length text))
      (push (subseq text start) lines))
    (coerce (nreverse lines) 'vector)))

(defun print-source-context (loc &key (stream *standard-output*) (context 1))
  (when (null loc) (return-from print-source-context nil))
  (let* ((file-key (source-loc-file loc))
         (text     (find-source-text file-key)))
    (when (null text) (return-from print-source-context nil))
    (let* ((lines     (%split-lines text))
           (nlines    (length lines))
           (start-ln  (source-loc-start-line loc))
           (start-col (source-loc-start-col loc))
           (end-ln    (source-loc-end-line loc))
           (end-col   (source-loc-end-col loc))
           (win-start (max 1 (- start-ln context)))
           (win-end   (min nlines (+ end-ln context)))
           (gutter    (length (format nil "~D" win-end))))
      (flet ((print-line (line-num)
               (let ((lt (if (and (>= line-num 1) (<= line-num nlines))
                             (aref lines (1- line-num)) "")))
                 (format stream "~&  ~vD | ~A~%" gutter line-num lt)))
             (print-carets (line-num)
               (let* ((lt (if (and (>= line-num 1) (<= line-num nlines))
                              (aref lines (1- line-num)) ""))
                      (ll (length lt))
                      (c-start (if (= line-num start-ln) (max 1 start-col) 1))
                      (c-end   (if (= line-num end-ln)
                                   (max c-start (1- end-col))
                                   (max c-start ll)))
                      (cc (max 1 (1+ (- c-end c-start)))))
                 (format stream "~&  ~vA | ~vA~A~%"
                         gutter "" (1- c-start) ""
                         (make-string cc :initial-element #\^)))))
        (loop for ln from win-start to win-end do
          (print-line ln)
          (when (and (>= ln start-ln) (<= ln end-ln))
            (print-carets ln)))))
    t))

(defun format-source-context (loc &key (context 1))
  (when loc
    (with-output-to-string (s)
      (unless (print-source-context loc :stream s :context context)
        (return-from format-source-context nil)))))

(defun source-context (form &key (context 1))
  (print-source-context (source-loc-or-ancestor form) :context context))

;; --- Test framework ---

(defvar *tests-run* 0) (defvar *tests-passed* 0) (defvar *failures* '())
(defmacro deftest (name &body body)
  `(progn (incf *tests-run*)
          (handler-case (progn ,@body (incf *tests-passed*)
                               (format t "  PASS ~A~%" ',name))
            (error (e) (push (cons ',name e) *failures*)
                       (format t "  FAIL ~A: ~A~%" ',name e)))))
(defun check (l e a)
  (unless (equalp e a) (error "~A:~%  expected: ~S~%  got:      ~S" l e a)))
(defun check-true (l x) (unless x (error "~A: expected true" l)))
(defun check-false (l x) (when x (error "~A: expected false, got ~S" l x)))
(defun has-substring (s sub)
  (search sub s :test #'char=))

;; --- Helper ---

(defun make-loc (file sl sc el ec)
  (make-source-loc :file file
                   :start-line sl :start-col sc
                   :end-line el :end-col ec
                   :start-offset 0 :end-offset 0))

;; --- Test source ---
;;
;; We register a known source text and build locs against it.

(defparameter *test-source*
  (format nil "(:module~%  (:block~%    (:assign (:temp x) :bogus :add 1 2)~%    (:ret))~%  (:global \"pi\" 3.14))"))
;; Lines (1-based):
;;  1: (:module
;;  2:   (:block
;;  3:     (:assign (:temp x) :bogus :add 1 2)
;;  4:     (:ret))
;;  5:   (:global "pi" 3.14))

;; --- Tests ---

(defun run-tests ()
  (setf *tests-run* 0 *tests-passed* 0 *failures* '())
  (clear-locs) (clear-texts)
  (register-source-text "test.blub" *test-source*)

  (format t "~&=== Basic context printing ===~%")

  (deftest single-line-span
    ;; Error on line 3, columns 5-43 (the whole :assign form).
    (let* ((loc (make-loc "test.blub" 3 5 3 43))
           (out (format-source-context loc)))
      (check-true "output non-nil" out)
      ;; Should contain line 2 (before), line 3 (error), carets, line 4 (after).
      (check-true "has line 2" (has-substring out "(:block"))
      (check-true "has line 3" (has-substring out ":assign"))
      (check-true "has line 4" (has-substring out ":ret"))
      (check-true "has carets" (has-substring out "^"))
      (format t "    Output:~%~A" out)))

  (deftest caret-alignment
    ;; Error on just the word :bogus on line 3 — cols 26-31.
    (let* ((loc (make-loc "test.blub" 3 26 3 32))
           (out (format-source-context loc)))
      (check-true "output non-nil" out)
      ;; Carets should be 6 wide (cols 26-31) and indented 25 spaces.
      (check-true "has carets" (has-substring out "^^^^^^"))
      ;; Shouldn't have a huge run of carets — max around 6.
      (check-false "not too many carets" (has-substring out "^^^^^^^^"))
      (format t "    Output:~%~A" out)))

  (deftest first-line-no-before-context
    ;; Error on line 1 — no line before to show.
    (let* ((loc (make-loc "test.blub" 1 1 1 9))
           (out (format-source-context loc)))
      (check-true "output non-nil" out)
      (check-true "has line 1" (has-substring out "(:module"))
      (check-true "has line 2" (has-substring out "(:block"))
      (check-true "has carets" (has-substring out "^"))
      (format t "    Output:~%~A" out)))

  (deftest last-line-no-after-context
    ;; Error on line 5 — no line after.
    (let* ((loc (make-loc "test.blub" 5 3 5 27))
           (out (format-source-context loc)))
      (check-true "has line 4" (has-substring out "(:ret"))
      (check-true "has line 5" (has-substring out ":global"))
      (check-true "has carets" (has-substring out "^"))
      (format t "    Output:~%~A" out)))

  (format t "~&=== Multi-line span ===~%")

  (deftest multi-line-span
    ;; Error spanning lines 2-4 (the whole :block).
    (let* ((loc (make-loc "test.blub" 2 3 4 11))
           (out (format-source-context loc)))
      (check-true "has line 1 context" (has-substring out "(:module"))
      (check-true "has line 2" (has-substring out "(:block"))
      (check-true "has line 3" (has-substring out ":assign"))
      (check-true "has line 4" (has-substring out "(:ret"))
      (check-true "has line 5 context" (has-substring out ":global"))
      ;; Should have carets on lines 2, 3, and 4.
      ;; Count caret lines: each caret line has ^ chars.
      (let ((caret-count (count #\^ out)))
        (check-true "multiple caret chars" (> caret-count 10)))
      (format t "    Output:~%~A" out)))

  (format t "~&=== More context ===~%")

  (deftest context-2
    ;; context=2 should show 2 lines before/after.
    (let* ((loc (make-loc "test.blub" 3 5 3 43))
           (out (format-source-context loc :context 2)))
      (check-true "has line 1" (has-substring out "(:module"))
      (check-true "has line 5" (has-substring out ":global"))
      (format t "    Output:~%~A" out)))

  (format t "~&=== Edge cases ===~%")

  (deftest nil-loc-returns-nil
    (check-false "nil" (format-source-context nil)))

  (deftest unknown-file-returns-nil
    (let ((loc (make-loc "nonexistent.blub" 1 1 1 5)))
      (check-false "no cached text" (format-source-context loc))))

  (deftest source-context-from-form
    ;; Test the convenience function that takes a form.
    (clear-locs)
    (let* ((form (list :bad :stuff))
           (loc  (make-loc "test.blub" 3 5 3 43)))
      (setf (source-loc form) loc)
      (let ((out (with-output-to-string (s)
                   (let ((*standard-output* s))
                     (source-context form :context 1)))))
        (check-true "produced output" (has-substring out ":assign"))
        (check-true "has carets" (has-substring out "^")))))

  (format t "~&=== Visual check (for eyeballing) ===~%")

  (deftest visual-check
    (format t "~%    --- Expected output for :bogus at line 3, cols 26-32 ---~%")
    (let ((loc (make-loc "test.blub" 3 26 3 32)))
      (print-source-context loc :stream *standard-output* :context 1))
    (format t "    --- end ---~%"))

  (format t "~&----~%~D/~D tests passed~%" *tests-passed* *tests-run*)
  (when *failures*
    (format t "Failures:~%")
    (dolist (f *failures*) (format t "  ~A: ~A~%" (car f) (cdr f))))
  (= *tests-passed* *tests-run*))

(unless (run-tests) (sb-ext:exit :code 1))
