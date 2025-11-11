;; -*- geiser-scheme-implementation: guile -*-

;; load the black scheme system 
(let ((cur-dir (getcwd)))
  (chdir "black")
  (load "guile.scm")
  (chdir cur-dir))


