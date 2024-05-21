
;; --------------------------------------------------------------------------------

(import scheme)

(import test)

(import (simple-loops))

(import (chicken io))
(import (chicken pretty-print))

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))

(import procedural-macros)
(import regex)

(import simple-md5)

(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val

;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors


;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(import matchable)

;;------------------------- code -----------------------------------

(include "fun.scm")

(define-macro (test! x y)
  `(test ,y ,x))


;; ----------------------- some unit tests -------------------------------

#|

cross
horz
vert ?
slash
backslash

moving left + cross -> depends on internal which direction to go next 
moving left + horz -> continue left
moving left + slash ->   /  move down
moving left + backslash -> \ move up 

|#


(test-group "train-shunt-left"
  
  (test!
   (train-shunt-left
    '((train-no 1) (x 3) (y 3) (direction left) (internal left)) 'cross)
    '((train-no 1) (x 2) (y 4) (direction down) (internal ahead))
   )


  (test!
   (train-shunt-left
    '((train-no 1) (x 3) (y 3) (direction left) (internal ahead)) 'cross)
    '((train-no 1) (x 1) (y 3) (direction left) (internal right))
   )


  (test!
   (train-shunt-left
    '((train-no 1) (x 3) (y 3) (direction left) (internal right)) 'cross)
    '((train-no 1) (x 2) (y 2) (direction up) (internal left))
   )

  ;; for each internal configuration
  (do-list (i '(right left ahead))
	   ;; horz does not affect internals
	   (test!
	    (begin
	      (train-shunt-left
	       `((train-no 1) (x 3) (y 3) (direction left) (internal ,i)) 'horz))
	       `((train-no 1) (x 2) (y 3) (direction left) (internal ,i)))
	   
	   (test!
	    (begin	      
	      (train-shunt-left
	       `((train-no 1) (x 3) (y 3) (direction left) (internal ,i)) 'slash))	    
	    `((train-no 1) (x 2) (y 4) (direction down) (internal ,i)))


	   (test!
	    (begin	      
	      (train-shunt-left
	       `((train-no 1) (x 3) (y 3) (direction left) (internal ,i)) 'backslash))	    
	    `((train-no 1) (x 2) (y 2) (direction up) (internal ,i)))

	   
	   
	   ) ;; do-list
  );; test-group




;; ----------------------------------------------------------------------------------------------------------

#|

cross
horz
vert ?
slash
backslash

moving right + cross -> depends on internal which direction to go next 
moving right + horz -> continue right
moving right + slash ->   /  move up
moving right + backslash -> \ move down 

|#

(test-group "train-shunt-right"
  
  (test!
   (train-shunt-right '((train-no 1) (x 3) (y 3) (direction right) (internal right)) 'cross)
   '((train-no 1) (x 4) (y 4) (direction down) (internal ahead))
   )


  #|
  (test!
   (train-shunt-right '((train-no 1) (x 3) (y 3) (direction right) (internal ahead)) 'cross)
   '((train-no 1) (x 1) (y 3) (direction right) (internal right))
   )


  (test!
   (train-shunt-right '((train-no 1) (x 3) (y 3) (direction right) (internal right)) 'cross)
   '((train-no 1) (x 2) (y 2) (direction up) (internal right))
   )

  ;; for each internal configuration
  (do-list (i '(right right ahead))
	   ;; horz does not affect internals
	   (test!
	    (begin
	      (train-shunt-right `((train-no 1) (x 3) (y 3) (direction right) (internal ,i)) 'horz)
	      )
	     `((train-no 1) (x 2) (y 3) (direction right) (internal ,i)))
	   
	   (test!
	    (begin	      
	      (train-shunt-right
	       `((train-no 1) (x 3) (y 3) (direction right) (internal ,i)) 'slash)
	      )	    
	    `((train-no 1) (x 2) (y 4) (direction down) (internal ,i)))
  ) ;; do-list

    |#

  );; test-group
  



;; --------------------------------------------------------------------------------------------------------




;; (define (test-train-shunt-right)
;; 
;;   (assert
;;    (equal?
;;     (train-shunt-right
;;      `((train-no 1) (x 3) (y 3) (direction right) (internal left)) 'cross)
;;      `((train-no 1) (x 4) (y 2) (direction up) (internal ahead))))
;; 
;;   (assert
;;    (equal?
;;     (train-shunt-right
;;      `((train-no 1) (x 3) (y 3) (direction right) (internal ahead)) 'cross)
;;      `((train-no 1) (x 1) (y 3) (direction left) (internal right))))
;;   ;; 
;;   ;; (assert
;;   ;;  (equal?
;;   ;;   (train-shunt-right
;;   ;;    `((train-no 1) (x 3) (y 3) (direction right) (internal right)) 'cross)
;;   ;;    `((train-no 1) (x 2) (y 2) (direction up) (internal left))))
;;   ;; 
;;   ;; ;; for each internal configuration
;;   ;; (do-list (i '(right left ahead))
;;   ;; 	   ;; horz does not affect internals
;;   ;; 	   (assert
;;   ;; 	    (equal?
;;   ;; 	     (train-shunt-right
;;   ;; 	      `((train-no 1) (x 3) (y 3) (direction right) (internal ,i)) 'horz)
;;   ;; 	      `((train-no 1) (x 2) (y 3) (direction left) (internal ,i))))
;;   ;; 	   
;;   ;; 	   (assert
;;   ;; 	    (equal?
;;   ;; 	     (train-shunt-right
;;   ;; 	      `((train-no 1) (x 3) (y 3) (direction right) (internal ,i)) 'slash)
;;   ;; 	      `((train-no 1) (x 2) (y 4) (direction down) (internal ,i))))
;;   ;; 	   
;;   ;; 	   )
;;   
;;   #t)
;; 
;; 
;; 
;; (define test-train-shunt-up #t)
;; 
;; (define test-train-shunt-down #t)

(test-exit)

