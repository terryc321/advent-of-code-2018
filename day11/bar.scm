
(import scheme)
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "day11")
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

;;(define input (get-input "input"))
;;(define input2 (get-input "input2"))
(define input 7857)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

;; ---------------------------------------------------------------------

#|

x 1 to 300
y 1 to 300
have to choose 3x3 grid square that has highest power 

|#



#|
bespoke 2d grid to save recomputing every time
|#

(define (make-2d-array width height)
  (let ((res (make-vector (+ height 2) #f)))
    (do-for (y 0 (+ height 1))
	    (let ((xvec (make-vector (+ width 2) #f)))
	      (vector-set! res y xvec)))
    res))

(define (set-2d v x y i)
  (let ((xvec (vector-ref v y)))
    (vector-set! xvec x i)))

(define (get-2d v x y)
  (let ((xvec (vector-ref v y)))
    (vector-ref xvec x)))


(define (test-2d)
  (let ((v (make-2d-array 2 2)))
    (set-2d v 1 1 1)
    (set-2d v 2 1 2)
    (set-2d v 1 2 3)
    (set-2d v 2 2 4)
    v))

;; --------------------
(define power-helper
  (lambda (x y serial debug)
    (let ((rackid 0)
	  (pwr 0)
	  (hundreds 0))
      (when debug (format #t "PoWER x: ~a :y ~a :serial ~a ~%" x y serial))
      (set! rackid (+ x 10))
      (when debug (format #t "(1) rackid = ~A ~%" rackid))
      (set! pwr (* rackid y))
      (when debug (format #t "(2) power level starts (:rackid ~a * :y ~a)  = ~A ~%" rackid y pwr))
      (let ((old-pwr pwr))
	(set! pwr (+ pwr serial))
	(when debug (format #t "(3) adding serial number produces (:pwr ~a + :serial ~a) starts = ~A ~%" old-pwr serial pwr))
	)
      (let ((old-pwr pwr))
	(set! pwr (* pwr rackid))
	(when debug (format #t "(4) multiplying by rackid (:pwr ~a * :rackid ~a)  = ~A ~%" 
			    old-pwr rackid pwr))
	)    
      (let* ((str (format #f "~a" pwr))(rev-str (list->string (reverse (string->list str)))))
	(when debug 
	  (format #t "str = ~a ~%" str)
	  (format #t "rev-str = ~a ~%" rev-str)
	  )
	(cond
	 ((>= (string-length str) 3)
	  (set! hundreds (string->number (format #f "~a" (string-ref rev-str 2)))))
	 (#t (set! hundreds 0)))
	(when debug
	  (format #t "(5) the hundreds digit of ~a = ~a ~%" str hundreds))
	(let ((res (- hundreds 5)))
	  (when debug
	    (format #t "(6) subtracting 5 produces ~a - 5 = ~a ~%" hundreds res))
	  res)))))


(define memo-on #f)

(define power 
  (let ((arr (make-2d-array 305 305)))
    (lambda (x y serial debug)
      (cond
       (memo-on 
	(let ((lookup (get-2d arr x y)))
	  (cond
	   (lookup lookup)
	   (#t (let ((res (power-helper x y serial debug)))
		 (set-2d arr x y res)
		 res)))))
       (#t
	(power-helper x y serial debug))))))


(define grid 
    (lambda (x1 x2 y1 y2 input debug)
      (let ((sum 0))
	(do-for (y y1 (+ y2 1))
		(when debug (format #t "~%"))
		(do-for (x x1 (+ x2 1))
			(let ((p (power x y input debug)))
			  (when debug (format #t "~a " p))
			  (set! sum (+ sum p))
			  )))
	(when debug (format #t "~%"))
	sum)))


    

		  
#|
test cases

    Fuel cell at  122,79, grid serial number 57: power level -5.
    Fuel cell at 217,196, grid serial number 39: power level  0.
    Fuel cell at 101,153, grid serial number 71: power level  4.
|#
(define (exercise debug)
  (let* ((x 3)(y 5)(serial 8)(pwr (power x y serial debug)))
    (format #t "ex1 : grid (~a,~a) : serial ~a => :power ~a : expect 4~%" x y serial pwr))
  (let* ((x 122)(y 79)(serial 57)(pwr (power x y serial debug)))
    (format #t "ex2 : grid (~a,~a) : serial ~a => :power ~a : expect -5~%" x y serial pwr))
  (let* ((x 217)(y 196)(serial 39)(pwr (power x y serial debug)))
    (format #t "ex3 : grid (~a,~a) : serial ~a => :power ~a : expect 0~%" x y serial pwr))
  (let* ((x 101)(y 153)(serial 71)(pwr (power x y serial debug)))
    (format #t "ex4 : grid (~a,~a) : serial ~a => :power ~a : expect 4~%" x y serial pwr))
)



(define (compute-powers debug)
  (let ((max-tot #f)
	(max-y 0)
	(max-x 0)
	(max-size 0)
	(xlim 300)
	(ylim 300)
	(serial input))
    ;; varying size of 
    (do-for (size 1 305)
	    (format #t "computing grid squares size ~a " size)
	    (do-for (x 1 (- xlim size))
		    (do-for (y 1 (- ylim size))
			    (cond 
			     ((and 
			       (>= x 1)
			       (>= y 1)
			       (>= (+ x (1- size)) 1)
			       (>= (+ y (1- size)) 1)
			       (<= x 300)
			       (<= y 300)		       
			       (<= (+ x (1- size)) 300)
			       (<= (+ x (1- size)) 300))
			      (let ((tot (grid x (+ x (1- size)) y (+ y (1- size)) serial debug)))
				(cond
				 (max-tot (when (> tot max-tot)
					    (set! max-size size)
					    (set! max-x x)
					    (set! max-y y)
					    (set! max-tot tot)))
				 (#t (set! max-x x)
				     (set! max-y y)
				     (set! max-size size)
				     (set! max-tot tot)))			
				;;(format #t "~%~%------------------------------------------------------~%~%")
				)))))
	    (format #t ": max x,y ~a ~a : max size ~a : max tot ~a ~%" max-x max-y max-size max-tot))
    (values 'x max-x 'y max-y 'size max-size 'tot max-tot)))




;; send it
;;(compute-powers #f)

(define (old-way)
  (set! memo-on #f)
  (compute-powers #f))

(define (new-way)
  (set! memo-on #t)
  (compute-powers #f))

(new-way)




#|

[terry@terry-allseries day11]$ time ./bar
computing grid squares size 1 : max x,y 1 4 : max size 1 : max tot 4 
computing grid squares size 2 : max x,y 21 271 : max size 2 : max tot 16 
computing grid squares size 3 : max x,y 243 16 : max size 3 : max tot 31 
computing grid squares size 4 : max x,y 243 14 : max size 4 : max tot 43 
computing grid squares size 5 : max x,y 243 12 : max size 5 : max tot 61 
computing grid squares size 6 : max x,y 243 10 : max size 6 : max tot 64 
computing grid squares size 7 : max x,y 243 10 : max size 6 : max tot 64 
computing grid squares size 8 : max x,y 237 231 : max size 8 : max tot 70 
computing grid squares size 9 : max x,y 235 232 : max size 9 : max tot 80 
computing grid squares size 10 : max x,y 235 229 : max size 10 : max tot 93 
computing grid squares size 11 : max x,y 233 229 : max size 11 : max tot 110 
computing grid squares size 12 : max x,y 233 229 : max size 12 : max tot 124 
computing grid squares size 13 : max x,y 233 229 : max size 12 : max tot 124 
computing grid squares size 14 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 15 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 16 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 17 : max x,y 231 227 : max size 14 : max tot 129 
...
...
...
computing grid squares size 268 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 269 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 270 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 271 : max x,y 231 227 : max size 14 : max tot 129 

...
...

computing grid squares size 302 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 303 : max x,y 231 227 : max size 14 : max tot 129 
computing grid squares size 304 : max x,y 231 227 : max size 14 : max tot 129 

real	87m56.888s
user	87m12.546s
sys	0m41.585s

didnt wait for it to finish ... 2 hours to compute solution...

231,227,14 
accepted solution 

|#








