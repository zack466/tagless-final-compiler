;;;; dev.lisp
(require "asdf")

(let* ((env (uiop:getenv "CL_SOURCE_REGISTRY"))
       (nix-dirs (when env
                   (loop for part in (uiop:split-string env :separator ":")
                         when (and part (not (string= part "")))
                         collect (list :tree
                                       (string-right-trim "/" part))))))
  (asdf:initialize-source-registry
   `(:source-registry
     ,@nix-dirs
     :ignore-inherited-configuration)))

(let ((asd-path (merge-pathnames "tagless-compiler.asd" *load-truename*)))
  (asdf:load-asd asd-path))

(asdf:load-system :tagless-compiler)
(format t "~%[OK] Tagless Compiler loaded successfully in ~A!~%" *package*)
