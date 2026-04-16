;;;; dev.lisp - Bootstrap script for development

(require "asdf")

;; 1. Nuke the global Nix registry so it doesn't crawl XDG_DATA_DIRS
(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))

;; 2. Dynamically find the .asd file relative to THIS script, no matter where it is cloned
(let ((asd-path (merge-pathnames "tagless-compiler.asd" *load-truename*)))
  (asdf:load-asd asd-path))

;; 3. Load the system (this will now be instantaneous)
(asdf:load-system :tagless-compiler)

(format t "~%[OK] Tagless Compiler loaded successfully in ~A!~%" *package*)
