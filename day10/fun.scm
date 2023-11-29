
(import scheme)
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "../day4")
;; (current-directory)



(import procedural-macros)
(import regex)

(import simple-md5)
(import simple-loops)

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

;; change input file ! 
(define (get-input f) (call-with-input-file f
		      (lambda (port)
			(read port))))

(define input (get-input "input"))
(define input2 (get-input "input2"))



(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;; ---------------------------------------------------------------------

#|

list of position x y velocity vx vy

|#

(define (bespoke-membership x y xs)
  (call/cc (lambda (escape)
	     (do-list (p xs)
		      (match p
			((x2 y2 vx2 vy2) (cond
					  ((and (= x x2) (= y y2)) (escape #t))))
			(_ (error "bespoke-membership"))))
	     #f)))



(define (tick xs n stop viz)
  (let ((min-x #f)
	(min-y #f)
	(max-x #f)
	(max-y #f))
    (let ((ys (map (lambda (e)
		     (match e 
		       ((x y vx vy)
			(let ((x2 (+ x vx))
			      (y2 (+ y vy)))
			  (when
			      (not min-x)
			    (set! min-x x2)
			    (set! max-x x2)
			    (set! min-y y2)
			    (set! max-y y2))
			  (when (< x2 min-x) (set! min-x x2))
			  (when (> x2 max-x) (set! max-x x2))
			  (when (< y2 min-y) (set! min-y y2))
			  (when (> y2 max-y) (set! max-y y2))			  
			  (list (+ x vx)
				(+ y vy)
				vx
				vy)))))
		   xs)))
     
      (format #t "tick ~a : width ~a : height ~a ~%" n (- max-x min-x) (- max-y min-y))
      
      (cond
       ((or viz (> n 10008)) #f ;;(or viz (> n 9995)) ;; visualiser 
	(do-for (y (- min-y 1) (+ 1 max-y))	 
		    (format #t "~%")
		    (do-for (x (- min-x 1) (+ 2 max-x) +1)
			    (cond
			     ;;((member (list x y) ys) (format #t "#"))
			     ((bespoke-membership x y ys) (format #t "#"))
			     (#t (format #t " ")))))	
	(format #t "~%~%")))
      
      (cond 
       ((= n stop)
	#f)
       (#t
	(tick ys (+ n 1) stop viz))))))





(define (example)
  (tick input2 1 3 #t))


(define (part-1)
  (tick input 1 10013 #f))

    

#|

tick 10011 : width 61 : height 9 

                                                                
 #       #####    ####   #####   #####   #    #  ######  ###### 
 #       #    #  #    #  #    #  #    #  #    #  #            # 
 #       #    #  #       #    #  #    #  #    #  #            # 
 #       #    #  #       #    #  #    #  #    #  #           #  
 #       #####   #       #####   #####   ######  #####      #   
 #       #  #    #  ###  #       #    #  #    #  #         #    
 #       #   #   #    #  #       #    #  #    #  #        #     
 #       #   #   #    #  #       #    #  #    #  #       #      
 #       #    #  #   ##  #       #    #  #    #  #       #      
 ######  #    #   ### #  #       #####   #    #  ######  ###### 


LRGPBHEZ

wait 10011 seconds



|#	 

