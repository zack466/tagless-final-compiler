(in-package #:tagless-compiler)

;; --- Source locations ---
;;
;; This module attaches line/column source information to cons cells read
;; from a stream, without changing the data structure (still plain Common
;; Lisp lists). The mechanism is a sidecar hash table keyed by EQ on cons
;; cells. Reading is done by Eclector, which gives us per-cons character
;; ranges. We post-process the ranges into (line, column) pairs by walking
;; the source text once.
;;
;; The propagating constructors (with-loc, inherit-loc, cons-loc, list-loc)
;; let handlers build new AST nodes that automatically carry the source
;; info of an existing node. This is what makes derived nodes traceable
;; back to original source even after many transformation passes.

;; --- The source-loc record and the sidecar table ---

(defstruct source-loc
  "Source location for a cons cell. FILE is a string or NIL. The four
   coordinates are 1-based for human-friendly display: START-LINE/START-COL
   point at the open paren, END-LINE/END-COL point at the character just
   past the close paren. START-OFFSET/END-OFFSET are the raw character
   offsets the reader returned, kept for clients that want them."
  file
  start-line
  start-col
  end-line
  end-col
  start-offset
  end-offset)

(defvar *source-locations* (make-hash-table :test #'eq)
  "Sidecar table: cons cell -> SOURCE-LOC. Populated by READ-WITH-LOCATIONS
   and by the propagating constructors. Lookups are O(1) and survive any
   non-destructive transformation.")

(defun source-loc (form)
  "Look up the source location for FORM, or NIL if none is recorded.
   Only conses can have entries — atoms always return NIL."
  (and (consp form) (gethash form *source-locations*)))

(defun (setf source-loc) (loc form)
  "Set or clear the source location for FORM. FORM must be a cons.
   LOC of NIL removes the entry."
  (unless (consp form)
    (error "Cannot attach a source location to a non-cons: ~S" form))
  (if loc
      (setf (gethash form *source-locations*) loc)
      (remhash form *source-locations*))
  loc)

(defun clear-source-locations ()
  "Drop all recorded source locations. Mostly useful between tests."
  (clrhash *source-locations*))

;; --- Pretty printer ---

(defun format-source-loc (loc &key (stream nil))
  "Format LOC as e.g. \"foo.lisp:3:5\" or \"foo.lisp:3:5-7:12\". If STREAM
   is NIL, returns a string; otherwise prints to STREAM and returns NIL."
  (cond ((null loc) (if stream (princ "<no location>" stream) "<no location>"))
        (t
         (let ((parts (list (or (source-loc-file loc) "<unknown>")
                            (source-loc-start-line loc)
                            (source-loc-start-col loc))))
           (if (or (/= (source-loc-start-line loc) (source-loc-end-line loc))
                   (/= (source-loc-start-col loc) (source-loc-end-col loc)))
               (format stream "~A:~D:~D-~D:~D"
                       (filename (first parts)) (second parts) (third parts)
                       (source-loc-end-line loc)
                       (source-loc-end-col loc))
               (format stream "~A:~D:~D"
                       (first parts) (second parts) (third parts)))))))

;; --- Reader: stream -> list, with source info populated ---
;;
;; Strategy: Eclector's CST reader gives us a tree where each node has
;; cst:raw (the cons we want) and cst:source (a (start . end) range of
;; character offsets returned by FILE-POSITION). We:
;;   1. Read the entire input into a string so we can map offsets to
;;      line/column by walking the text once.
;;   2. Run eclector.concrete-syntax-tree:read on a string stream.
;;   3. Walk the CST and populate *source-locations* keyed by the raw
;;      conses.
;;
;; The string-buffering step is unfortunate for very large files but
;; lets us do everything portably, and matters of "very large" rarely
;; apply to source code.

(defun %build-line-table (text)
  "Build a vector V of length (number-of-lines) where V[i] is the
   character offset of the start of line (i+1). Used to translate char
   offsets to line/column in O(log lines) per lookup. We deliberately
   omit a sentinel for the past-end position: looking up an offset past
   the last newline naturally falls onto the last real line, which is
   what we want for end-of-range positions (they get a column equal to
   one past the last character of that line)."
  (let ((starts (list 0)))
    (loop for i from 0 below (length text) do
      (when (char= (char text i) #\Newline)
        (push (1+ i) starts)))
    (coerce (nreverse starts) 'vector)))

(defun %offset-to-line-col (offset line-starts)
  "Translate character OFFSET into (values LINE COL), both 1-based.
   COL is column of the character at OFFSET — i.e. number of characters
   on that line before it, plus one."
  ;; Binary search for the largest line-start <= offset.
  (let ((lo 0) (hi (1- (length line-starts))))
    (loop while (< lo hi) do
      (let ((mid (floor (+ lo hi 1) 2)))
        (if (<= (aref line-starts mid) offset)
            (setf lo mid)
            (setf hi (1- mid)))))
    (values (1+ lo)
            (1+ (- offset (aref line-starts lo))))))

(defun %range-to-source-loc (file range line-starts)
  "Convert a (START . END) char-offset range into a SOURCE-LOC."
  (when range
    (multiple-value-bind (sl sc) (%offset-to-line-col (car range) line-starts)
      (multiple-value-bind (el ec) (%offset-to-line-col (cdr range) line-starts)
        (make-source-loc :file         file
                         :start-line   sl
                         :start-col    sc
                         :end-line     el
                         :end-col      ec
                         :start-offset (car range)
                         :end-offset   (cdr range))))))

(defun %walk-cst (cst file line-starts)
  "Walk CST and populate *SOURCE-LOCATIONS* for every cons in (cst:raw cst).
   Returns the raw form for the top-level CST."
  (cond
    ((typep cst 'concrete-syntax-tree:cons-cst)
     (let* ((raw   (concrete-syntax-tree:raw cst))
            (range (concrete-syntax-tree:source cst))
            (loc   (%range-to-source-loc file range line-starts)))
       (when (and loc (consp raw))
         ;; Only set if we don't already have an entry — this preserves
         ;; the *outermost* (most informative) location if a cons is
         ;; somehow shared across multiple parses, which shouldn't happen
         ;; from a normal read but is cheap insurance.
         (unless (gethash raw *source-locations*)
           (setf (gethash raw *source-locations*) loc)))
       ;; Recurse into children. cst:first/cst:rest give us the head and
       ;; tail CST nodes; we walk both. For dotted lists, cst:rest of the
       ;; last cons-cst is an atom-cst, so the recursion bottoms out.
       (%walk-cst (concrete-syntax-tree:first cst) file line-starts)
       (%walk-cst (concrete-syntax-tree:rest  cst) file line-starts)
       raw))
    ((typep cst 'concrete-syntax-tree:atom-cst)
     ;; Atoms don't get entries (they're often EQ-shared, e.g. symbols
     ;; like NIL or numbers, and would collide with each other across
     ;; reads). We still return their raw value.
     (concrete-syntax-tree:raw cst))
    (t cst)))

(defun %slurp-stream (stream)
  "Read all remaining characters from STREAM into a fresh string."
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
          while c do (write-char c out))))

;; --- Patch for source-locations.lisp ---
(defun read-all-with-locations (stream &key (file nil))
  "Read all remaining forms from STREAM, populating *SOURCE-LOCATIONS*
   for every cons in the result. Also caches the source text in
   *SOURCE-TEXTS* (keyed by FILE) so that PRINT-SOURCE-CONTEXT can
   display contextual snippets around errors."
  (let* ((text        (%slurp-stream stream))
         (line-starts (%build-line-table text))
         (forms       '())
         (eof         (load-time-value (cons :eof :sentinel)))
         ;; Use a canonical key for string inputs without an explicit file.
         (file-key    (or file "<string>")))
    ;; Cache the source text for context printing.
    (register-source-text file-key text)
    (with-input-from-string (s text)
      (loop
        (let ((cst (eclector.concrete-syntax-tree:read s nil eof)))
          (when (eq cst eof) (return))
          (push (%walk-cst cst file-key line-starts) forms))))
    (nreverse forms)))

(defun read-from-string-with-locations (string &key (file nil))
  "Read all forms from STRING. Returns a list of forms; cons cells in
   the result carry source locations in *SOURCE-LOCATIONS*. FILE is an
   optional label (typically a filename) attached to each location."
  (with-input-from-string (s string)
    (read-all-with-locations s :file file)))

(defun read-file-with-locations (pathname)
  "Read all forms from the file at PATHNAME, using its namestring as
   the FILE for source-locs. Returns a list of forms."
  (with-open-file (s pathname :direction :input)
    (read-all-with-locations s :file (namestring pathname))))

(defun example-pathname (name)
  "Resolve NAME (a string like \"hello.lisp\") to a pathname inside
   the project's examples/ directory."
  (asdf:system-relative-pathname "tagless-compiler"
                                 (format nil "examples/~A" name)))

(defun read-example (name)
  "Read forms with locations from examples/NAME."
  (read-file-with-locations (example-pathname name)))

;; --- Propagating constructors ---
;;
;; These let handlers build new AST nodes that inherit a source location
;; from an existing node. The basic idea: most derived nodes correspond
;; to *some* original node — a (:declare (int x) value) lowering reads
;; from one input and produces some output, and we want the output (and
;; any subforms) to point back at the original input.
;;
;; with-loc and inherit-loc are the two main entry points. The list-loc
;; / cons-loc helpers are sugar for the common "build a fresh list/cons
;; that inherits from this source" pattern.

(defun inherit-loc (target source)
  "Copy SOURCE's source-loc onto TARGET, if SOURCE has one and TARGET
   doesn't already have one. TARGET must be a cons (atoms can't carry
   loc info). Returns TARGET. Idempotent on TARGET — it never overwrites
   an existing loc, so the *first* attribution wins. (If you want to
   forcibly retag, use (setf (source-loc target) ...) directly.)"
  (when (consp target)
    (let ((src-loc (source-loc source)))
      (when (and src-loc (not (gethash target *source-locations*)))
        (setf (gethash target *source-locations*) src-loc))))
  target)

(defun cons-loc (car cdr source)
  "Like CONS, but the resulting cons inherits its location from SOURCE."
  (inherit-loc (cons car cdr) source))

(defun list-loc (source &rest items)
  "Like LIST, but the resulting list's first cons inherits its location
   from SOURCE. Note: only the *outermost* cons of the new list gets a
   loc — interior conses (the cdrs) are bare. This is intentional and
   matches how loc info is typically used: you ask for the location of
   a node, not of its tail."
  (inherit-loc (copy-list items) source))

(defmacro with-loc ((source) &body body)
  "Evaluate BODY; if its value is a cons with no source-loc yet,
   attribute it to SOURCE. Useful as a wrapper around handler bodies:
     (def-op interp (:foo a b)
       (with-loc ((expr))
         (list :bar (recurse a) (recurse b))))
   This way the produced (:bar ...) inherits the location of the input
   (:foo ...) without the handler having to thread it through manually."
  (let ((src (gensym "SRC"))
        (val (gensym "VAL")))
    `(let* ((,src ,source)
            (,val (progn ,@body)))
       (inherit-loc ,val ,src))))

;; --- Walking up to find a location ---
;;
;; When a node has no direct location entry (because it was constructed
;; by hand without an inherit-loc call, or it's an atom inside a located
;; cons), we'd still like to give the user a useful answer. The "nearest
;; located ancestor" approach requires knowing the ancestors, which a
;; bare cons doesn't carry. So we do the next best thing: scan the
;; sidecar table for any cons whose recorded range contains this one.
;; That's O(table-size) per lookup, so we only do it on demand (e.g.,
;; in error reporting), not on every access.

(defun source-loc-or-ancestor (form)
  "Return a SOURCE-LOC for FORM, falling back to a nearby located form
   if FORM itself has no entry. The fallback is the smallest located
   range that fully contains FORM, by character offset, when FORM has
   a usable offset of its own (i.e. some recorded sub-form). Returns
   NIL if nothing is findable."
  (or (source-loc form)
      (let ((self-offset (some-recorded-offset form)))
        (when self-offset
          (smallest-containing-range self-offset)))))

(defun some-recorded-offset (form)
  "Walk FORM's structure looking for any sub-cons that has a recorded
   loc; return its start-offset, or NIL. Used as a probe by
   SOURCE-LOC-OR-ANCESTOR."
  (cond ((not (consp form)) nil)
        ((source-loc form) (source-loc-start-offset (source-loc form)))
        (t (or (some-recorded-offset (car form))
               (some-recorded-offset (cdr form))))))

(defun smallest-containing-range (offset)
  "Find the smallest recorded SOURCE-LOC whose offset range contains
   OFFSET, or NIL if none does. Linear in the table size."
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

;; --- Source text cache ---
;;
;; *SOURCE-TEXTS* maps file identifiers (the FILE argument passed to
;; READ-ALL-WITH-LOCATIONS) to the source text string. Populated at
;; read time, used at error-reporting time to print contextual
;; snippets. Strings passed in by READ-FROM-STRING-WITH-LOCATIONS use
;; an interned key ("<string>").

(defvar *source-texts* (make-hash-table :test #'equal)
  "File-key -> source-text string. Populated by the locating reader.")

(defun register-source-text (file-key text)
  "Cache TEXT under FILE-KEY in *SOURCE-TEXTS*. Called by the reader."
  (setf (gethash file-key *source-texts*) text))

(defun find-source-text (file-key)
  "Look up cached source text for FILE-KEY, or NIL if not found."
  (gethash file-key *source-texts*))

(defun clear-source-texts ()
  "Drop all cached source texts."
  (clrhash *source-texts*))

;; --- Context snippet printer ---
;;
;; PRINT-SOURCE-CONTEXT takes a SOURCE-LOC and prints a few lines of
;; surrounding context with carets pointing at the relevant span.
;;
;; Example output for an error at line 3, columns 5-15:
;;
;;    2 |   (:block
;;    3 |     (:assign (:temp x) :bogus :add 1 2)
;;      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;    4 |     (:ret))
;;
;; The number of context lines before/after is controlled by the CONTEXT
;; keyword (default 1). Line numbers are right-justified to align the
;; gutter.

(defun %split-lines (text)
  "Split TEXT into a vector of line strings (without newlines)."
  (let ((lines '())
        (start 0))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (push (subseq text start i) lines)
               (setf start (1+ i)))
    ;; Last line (no trailing newline).
    (when (<= start (length text))
      (push (subseq text start) lines))
    (coerce (nreverse lines) 'vector)))

(defun print-source-context (loc &key (stream *standard-output*)
                                      (context 1))
  "Print a contextual source snippet for LOC to STREAM. Shows CONTEXT
   lines before and after the error line, with carets under the span.
   Returns NIL if the source text isn't cached or LOC is NIL.

   If LOC spans multiple lines, carets cover from START-COL to end of
   the start line, then full-width on intermediate lines, then from
   column 1 to END-COL on the last line."
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
      
      ;; Guarantee we start on a fresh line exactly ONCE
      (fresh-line stream)
      (format stream "~A:~%" (filename file-key))
      
      (labels ((get-line-text (ln)
                 (if (and (>= ln 1) (<= ln nlines))
                     (string-right-trim '(#\Newline #\Return) (aref lines (1- ln)))
                     ""))
               (print-line (line-num)
                 ;; Removed ~& to avoid double-spacing on Gray streams
                 (format stream "  ~vD | ~A~%" gutter line-num
                         (highlight-lisp (get-line-text line-num))))
               (print-carets (line-num)
                 (let* ((line-text (get-line-text line-num))
                        (line-len  (length line-text))
                        (c-start (if (= line-num start-ln) (max 1 start-col) 1))
                        (c-end   (if (= line-num end-ln)
                                     (max c-start (1- end-col))
                                     (max c-start line-len)))
                        (caret-count (max 1 (1+ (- c-end c-start)))))
                   ;; Removed ~& here as well
                   (format stream "  ~vA | ~vA~A~%"
                           gutter ""
                           (1- c-start) ""
                           (gutter (make-string caret-count :initial-element #\^))))))
        (loop for ln from win-start to win-end do
          (print-line ln)
          (when (and (>= ln start-ln) (<= ln end-ln))
            (print-carets ln)))))
    t))

(defun format-source-context (loc &key (context 1))
  "Like PRINT-SOURCE-CONTEXT but returns a string (or NIL)."
  (when loc
    (with-output-to-string (s)
      (unless (print-source-context loc :stream s :context context)
        (return-from format-source-context nil)))))

(defun source-context (form &key (stream *standard-output*) (context 1))
  "Convenience: look up FORM's source-loc (or ancestor) and print context."
  (print-source-context (source-loc-or-ancestor form)
                        :stream stream :context context))
