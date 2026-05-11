(in-package #:tagless-compiler)

(defvar *test-source*
  "(:module
  (:block
    (:assign x (:add (:var x) 1))
    (:ret (:var x)))
  (:global (:type :int) z 5))")

(defun make-loc (file sl sc el ec)
  (make-source-loc :file file
                   :start-line sl :start-col sc
                   :end-line el :end-col ec
                   :start-offset 0 :end-offset 0))

(defun format-source-context (loc &key (context 1))
  (when loc
    (let* ((success nil)
           (out (with-output-to-string (s)
                  (setf success (print-source-context loc :stream s :context context)))))
      (when success
        (cl-ppcre:regex-replace-all "\\e\\[[0-9;]*m" out "")))))

(defun has-substring (string sub)
  (and string (search sub string)))

(defsuite "source-context printer: basic"
  (deftest "single-line span includes context lines and carets"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((loc (make-loc "test.blub" 3 5 3 43))
           (out (format-source-context loc)))
      (check-true out)
      (check-true (has-substring out "(:block"))
      (check-true (has-substring out ":assign"))
      (check-true (has-substring out ":ret"))
      (check-true (has-substring out "^"))))

  (deftest "caret alignment"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((loc (make-loc "test.blub" 3 26 3 32))
           (out (format-source-context loc)))
      (check-true out)
      (check-true (has-substring out "^^^^^^"))
      (check-false (has-substring out "^^^^^^^^"))))

  (deftest "first-line: no line before context"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((loc (make-loc "test.blub" 1 1 1 9))
           (out (format-source-context loc)))
      (check-true out)
      (check-true (has-substring out "(:module"))
      (check-true (has-substring out "(:block"))
      (check-true (has-substring out "^"))))

  (deftest "last-line: no line after context"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((loc (make-loc "test.blub" 5 3 5 27))
           (out (format-source-context loc)))
      (check-true (has-substring out ":ret"))
      (check-true (has-substring out ":global"))
      (check-true (has-substring out "^")))))

(defsuite "source-context printer: multi-line"
  (deftest "multi-line span shows all spanned lines with carets"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((loc (make-loc "test.blub" 2 3 4 11))
           (out (format-source-context loc)))
      (check-true (has-substring out "(:module"))
      (check-true (has-substring out "(:block"))
      (check-true (has-substring out ":assign"))
      (check-true (has-substring out ":ret"))
      (check-true (has-substring out ":global"))
      (check-true (> (count #\^ out) 10))))

  (deftest "context=2 extends window"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((loc (make-loc "test.blub" 3 5 3 43))
           (out (format-source-context loc :context 2)))
      (check-true (has-substring out "(:module"))
      (check-true (has-substring out ":global")))))

(defsuite "source-context printer: edge cases"
  (deftest "nil loc returns nil"
    (check-false (format-source-context nil)))

  (deftest "unknown file returns nil"
    (clrhash tagless-compiler::*source-texts*)
    (let ((loc (make-loc "nonexistent.blub" 1 1 1 5)))
      (check-false (format-source-context loc))))

  (deftest "source-context from form"
    (clrhash tagless-compiler::*source-texts*)
    (setf (gethash "test.blub" tagless-compiler::*source-texts*) *test-source*)
    (let* ((form (list :bad :stuff))
           (loc  (make-loc "test.blub" 3 5 3 43)))
      (setf (source-loc form) loc)
      (let ((out (with-output-to-string (s)
                   (print-source-context (source-loc-or-ancestor form) :stream s :context 1))))
        (setf out (cl-ppcre:regex-replace-all "\\e\\[[0-9;]*m" out ""))
        (check-true (has-substring out ":assign"))
        (check-true (has-substring out "^"))))))
