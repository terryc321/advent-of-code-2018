;; -*- geiser-scheme-implementation: guile -*-

#|

(add-to-load-path "/home/terry/code/advent-code/advent-of-code-2018/day15/guile")

%load-path
$6 = ("/home/terry/code/advent-code/advent-of-code-2018" "/home/terry/.emacs.d/elpa/geiser-guile-0.28.3/src" "/opt/guile/share/guile/3.0" "/opt/guile/share/guile/site/3.0" "/opt/guile/share/guile/site" "/opt/guile/share/guile")
scheme@(#{ g405}#) [1]> ,q
scheme@(guile-user)> (getcwd)
$7 = "/home/terry/code/advent-code/advent-of-code-2018"
scheme@(guile-user)> (chdir "day15/guile")
scheme@(guile-user)> (getcwd)
$8 = "/home/terry/code/advent-code/advent-of-code-2018/day15/guile"
scheme@(guile-user)> (add-to-load-path (getcwd))
scheme@(guile-user)> %load-path
$9 = ("/home/terry/code/advent-code/advent-of-code-2018/day15/guile" "/home/terry/code/advent-code/advent-of-code-2018" "/home/terry/.emacs.d/elpa/geiser-guile-0.28.3/src" "/opt/guile/share/guile/3.0" "/opt/guile/share/guile/site/3.0" "/opt/guile/share/guile/site" "/opt/guile/share/guile")
scheme@(guile-user)> 

|#




;; for guile emacs geiser
;; this init file can alter the load paths
;;
;; we had to resort to setting DEVELOPER in bashrc so we can find out the toplevel directory of this
;; entire project
;;
;; dirty dirty hacks
;;

;; we can set LTDL_LIBRARY_PATH in environment outside emacs - in its terminal
;; so that takes care of feeeding environment varialbes around

;; (let ((toplevel (getenv "DEVELOPER")))
;;   (add-to-load-path toplevel))

;; (use-modules (system foreign-library))
;; (load-foreign-library (string-append (getenv "DEVELOPER") "/pixelformat/" "libpixelformat.so"))


;; for libpixelformat.so in $DEVELOPER/pixelformat directory
;;(putenv "LTDL_LIBRARY_PATH" (string-append (getenv "DEVELOPER") "/pixelformat/"))


;; geiser set 
;; (setq geiser-guile-init-file (concat (getenv "DEVELOPER") "/" "geiser-guile-init.scm"))
;;
;;
;; how does emacs get environemnt variables
;;
;;
;;      LTDL_LIBRARY_PATH={ some directory }
;;         DEVELOPER={some direrctory }
;; BASH -> EMACS -> GEISER -> guile-init.scm -> GUILE  ->  GUI ???
;;
;;
;;

  
