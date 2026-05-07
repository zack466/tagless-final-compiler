(in-package #:tagless-compiler)

(defvar *unbound-env* '#:unbound-env)

(defmacro define-pass-context (name &key doc)
  "Defines a dynamic variable to hold a compiler pass context (an immutable map)."
  `(defvar ,name *unbound-env* ,@(if doc (list doc) nil)))

(defmacro with-empty-scope ((&rest names) &body body)
  "Evaluates BODY with the given context variables bound to fresh, empty immutable maps."
  `(let ,(mapcar (lambda (n) `(,n (fset:empty-map))) names)
     ,@body))

(defmacro with-scope ((&rest names) &body body)
  "Evaluates BODY in a new dynamic scope where the given context variables are locally shadowed.
If any of the context variables are unbound (not initialized by WITH-EMPTY-SCOPE), signals an error."
  `(progn
     ,@(mapcar (lambda (n)
                 `(when (eq ,n *unbound-env*)
                    (error "Context variable ~A has not been initialized. Use WITH-EMPTY-SCOPE first." ',n)))
               names)
     (let ,(mapcar (lambda (n) `(,n ,n)) names)
       ,@body)))

(defmacro env-bind (name key value)
  "Adds or updates KEY with VALUE in the context NAME within the current dynamic scope. Returns VALUE."
  (let ((v (gensym)))
    `(let ((,v ,value))
       (setf ,name (fset:with ,name ,key ,v))
       ,v)))

(defmacro env-lookup (name key)
  "Looks up KEY in the context NAME. Returns (values value present-p)."
  `(fset:lookup ,name ,key))
