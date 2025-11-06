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

;; no srfi-25 instead guile has inbuilt procedures 
;; should be a 2d array indexes starting from 1..2 , 1..2 
(define arr #2@1@1((1 2) (3 4)))
(array-ref arr 1 1) ;; expect 1
(array-ref arr 2 1) ;; expect 3
(array-ref arr 1 2) ;; expect 2
(array-ref arr 2 2) ;; expect 4

;; all correct 

