(in-package #:tagless-compiler)

;; ---------------------------------------------------------------------------
;; Color utilities
;; ---------------------------------------------------------------------------
;; Thin wrappers over cl-ansi-text so call sites read like
;;   (red "error:") instead of (cl-ansi-text:red "error:")
;; and so we have a single place to add :effect :bright, disable color, etc.

(defparameter *use-color* t
  "Global toggle for colorization. When NIL, all colorize-* helpers and
   the named color functions return their input unchanged. Useful for
   piping output to files or for terminals that don't support ANSI.")

(defun colorize (string color &key effect)
  "Wrap STRING in ANSI escapes for COLOR. COLOR is a keyword like :red,
   :green, :blue, :yellow, :cyan, :magenta, :white, :black. EFFECT is an
   optional keyword such as :bright, :underline, or :inverse. Returns
   STRING unchanged when *USE-COLOR* is NIL or STRING is empty."
  (if (or (not *use-color*) (zerop (length string)))
      string
      (with-output-to-string (s)
        (if effect
            (cl-ansi-text:with-color (color :stream s :effect effect)
              (princ string s))
            (cl-ansi-text:with-color (color :stream s)
              (princ string s))))))

(defun colorize-bg (string color &key effect)
  "Like COLORIZE but sets the background color instead of the foreground."
  (if (or (not *use-color*) (zerop (length string)))
      string
      (with-output-to-string (s)
        (if effect
            (cl-ansi-text:with-color (color :stream s
                                            :effect effect
                                            :style :background)
              (princ string s))
            (cl-ansi-text:with-color (color :stream s
                                            :style :background)
              (princ string s))))))

;; Convenience shorthands for the common cases. Each accepts an optional
;; :bright flag rather than the more verbose :effect :bright since that's
;; the only effect we use 99% of the time.

(defun red     (s &key bright) (colorize s :red     :effect (when bright :bright)))
(defun green   (s &key bright) (colorize s :green   :effect (when bright :bright)))
(defun yellow  (s &key bright) (colorize s :yellow  :effect (when bright :bright)))
(defun blue    (s &key bright) (colorize s :blue    :effect (when bright :bright)))
(defun magenta (s &key bright) (colorize s :magenta :effect (when bright :bright)))
(defun cyan    (s &key bright) (colorize s :cyan    :effect (when bright :bright)))
(defun white   (s &key bright) (colorize s :white   :effect (when bright :bright)))
(defun gray    (s)             (colorize s :white)) ; dim white reads as gray on most terminals

(defun bold      (s) (if *use-color* (cl-ansi-text:white s :effect :bright) s))
(defun underline (s) (if *use-color* (format nil "~C[4m~A~C[0m" #\Esc s #\Esc) s))

;; Semantic helpers — use these in diagnostic output so the color scheme
;; lives in exactly one place. If you later decide errors should be magenta
;; instead of red, you change it here and not at every call site.

(defun severity-color (severity string)
  "Color STRING according to a diagnostic SEVERITY keyword:
   :error -> bright red, :warning -> bright yellow, :note -> bright cyan,
   :hint -> bright green. Unknown severities return STRING unchanged."
  (case severity
    (:error   (red    string :bright t))
    (:warning (yellow string :bright t))
    (:note    (cyan   string :bright t))
    (:hint    (green  string :bright t))
    (t        string)))

(defun gutter (string)
  "Color a source-listing gutter element (line numbers, the '|' separator,
   carets). Bright red so it stands out against the highlighted source."
  (red string :bright t))

(defun %shorten-path (path)
  "Return a display-friendly form of PATH. If PATH is under the current
   working directory, return it relative to CWD (without a leading './').
   Otherwise return the absolute namestring unchanged. Non-pathname or
   unparseable inputs are coerced to a string and returned as-is."
  (handler-case
      (let* ((path-pn (pathname path))
             ;; Resolve to absolute so a relative input still gets compared
             ;; against CWD on equal footing.
             (abs-pn  (merge-pathnames path-pn (uiop:getcwd)))
             (cwd     (uiop:getcwd)))
        (if (uiop:subpathp abs-pn cwd)
            (namestring (uiop:enough-pathname abs-pn cwd))
            (namestring abs-pn)))
    (error ()
      ;; Anything weird (a string that isn't a valid path, etc.) — fall
      ;; back to the original input. Diagnostics should never themselves
      ;; throw.
      (princ-to-string path))))

(defun filename (string)
  "Color a filename in diagnostic output. If the path is under the current
   working directory, it's shown relative to CWD; otherwise the full
   absolute path is shown. The result is underlined cyan."
  (underline (cyan (%shorten-path string))))

;; ---------------------------------------------------------------------------
;; Syntax highlighting
;; ---------------------------------------------------------------------------
;; Regex-based highlighter for s-expression source.
;; - takes in a string representation of s-expressions, and adds colors using ansi colors
;; - different color for keywords, specific keywords, strings, numbers, nil, t, etc
;; All colorization routes through the helpers above, which honor *use-color*.

(defparameter *special-keywords*
  '(":module" ":global" ":function" ":block" ":declare"
    ":assign" ":add" ":sub" ":mul" ":div" ":var" ":if"
    ":while" ":return" ":call" ":def" ":let" ":lambda")
  "Keywords that get a distinct highlight color (the form-introducing ones).")

(defparameter *type-keywords*
  '("int" "double" "float" "char" "void" "bool" "long" "short")
  "Type names from the source language.")

(defparameter *builtin-constants*
  '("nil" "t" "T" "NIL")
  "Lisp constants that get their own color.")

;; Token regex - order matters in the alternation since we scan left-to-right
;; and take the first match at each position.
(defparameter *lisp-token-scanner*
  (cl-ppcre:create-scanner
   (concatenate 'string
                "(\"(?:\\\\.|[^\"\\\\])*\")"          ; 1: string literal
                "|(;[^\\n]*)"                         ; 2: line comment
                "|(#\\\\(?:[A-Za-z]+|.))"             ; 3: character literal
                "|(:[A-Za-z_][A-Za-z0-9_-]*)"         ; 4: keyword (:foo)
                "|([-+]?[0-9]+\\.[0-9]+(?:[eE][-+]?[0-9]+)?)" ; 5: float
                "|([-+]?[0-9]+)"                      ; 6: integer
                "|([()])"                             ; 7: paren
                "|([A-Za-z_][A-Za-z0-9_!?*+/<>=-]*)"  ; 8: symbol
                ))
  "Master scanner that captures all token classes in one pass.")

(defun %colorize-symbol (sym)
  "Color a bare symbol based on whether it's a known type, constant, or other.
   Plain user-defined symbols are returned unchanged."
  (cond
    ((member sym *type-keywords* :test #'string=)
     (cyan sym))
    ((member sym *builtin-constants* :test #'string=)
     (magenta sym :bright t))
    (t
     sym)))

(defun %colorize-keyword (kw)
  "Color a :keyword token. Special form-introducing keywords get bright blue;
   ordinary keywords get plain blue."
  (if (member kw *special-keywords* :test #'string=)
      (blue kw :bright t)
      (blue kw)))

(defun highlight-lisp (source)
  "Return a copy of SOURCE with ANSI color escapes inserted for syntax
   highlighting. When *USE-COLOR* is NIL, return SOURCE unchanged so callers
   can uniformly invoke this without branching at every call site.

   Recognizes: string literals, line comments, character literals,
   :keywords (with extra emphasis on language-special ones), integer and
   float numeric literals, type names, NIL/T, parentheses, and ordinary
   symbols. Whitespace and unrecognized characters pass through verbatim."
  (unless *use-color*
    (return-from highlight-lisp source))
  (with-output-to-string (out)
    (let ((pos 0)
          (len (length source)))
      (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends
                                      *lisp-token-scanner* source)
        ;; Emit any non-token characters between the previous position and
        ;; this match (whitespace, stray punctuation we don't tokenize).
        (when (> match-start pos)
          (write-string source out :start pos :end match-start))
        (let ((token (subseq source match-start match-end)))
          (cond
            ;; String literal - group 1
            ((aref reg-starts 0)
             (write-string (green token) out))
            ;; Line comment - group 2
            ((aref reg-starts 1)
             (write-string (gray token) out))
            ;; Character literal - group 3
            ((aref reg-starts 2)
             (write-string (green token) out))
            ;; :keyword - group 4
            ((aref reg-starts 3)
             (write-string (%colorize-keyword token) out))
            ;; Float - group 5
            ((aref reg-starts 4)
             (write-string (yellow token) out))
            ;; Integer - group 6
            ((aref reg-starts 5)
             (write-string (yellow token) out))
            ;; Paren - group 7
            ((aref reg-starts 6)
             (write-string (white token :bright t) out))
            ;; Symbol - group 8
            ((aref reg-starts 7)
             (write-string (%colorize-symbol token) out))
            (t
             (write-string token out))))
        (setf pos match-end))
      ;; Trailing tail after the last match.
      (when (< pos len)
        (write-string source out :start pos :end len)))))

(defun lisp-to-string (expr)
  "Converts an sexp to a string before highlighting it"
  (highlight-lisp (format nil "~S" expr)))
