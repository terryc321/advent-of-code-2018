;; -*- geiser-scheme-implementation: guile -*-

#|

enjoy experience trying to load my own sdl ffi library into guile scheme - from game1 

load + compile game1.scm first , since everything is practically at toplevel and open 

|#


(use-modules (ice-9 optargs)) ;; optional args
(use-modules (system foreign)) ;; %null-pointer
(use-modules (system foreign-library))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print)) 
(define pp pretty-print)

;; these modules are not in guile ecosystem they require an altered %load-path to make them visible
(use-modules (macros fcase))
(use-modules (macros inc))
(use-modules ((graphics sdl2 sdl) #:prefix sdl:))
(use-modules ((graphics sdl2 image) #:prefix img:))
(use-modules ((graphics cairo cairo) #:prefix cairo:))

