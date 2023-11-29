
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
      
(define (power x y serial debug)
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
	res))))



(define (grid x1 x2 y1 y2 input debug)
  (let ((sum 0))
    (do-for (y y1 (+ y2 1))
	    (when debug (format #t "~%"))
	    (do-for (x x1 (+ x2 1))
		    (let ((p (power x y input debug)))
		      (when debug (format #t "~a " p))
		      (set! sum (+ sum p))
		    )))
    (when debug (format #t "~%"))
    sum))

    

		  
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
	(xlim 300)
	(ylim 300)
	(serial input))
    (do-for (x 1 (- xlim 2))
	    (do-for (y 1 (- ylim 2))
		    (cond 
		     ((and 
		       (>= x 1)
		       (>= y 1)
		       (>= (+ x 2) 1)
		       (>= (+ y 2) 1)
		       (<= x 300)
		       (<= y 300)		       
		       (<= (+ x 2) 300)
		       (<= (+ y 2) 300))
		      (let ((tot (grid x (+ x 2) y (+ y 2) serial debug)))
			(cond
			 (max-tot (when (> tot max-tot)
				    (set! max-x x)
				    (set! max-y y)
				    (set! max-tot tot)))
			 (#t (set! max-x x)
			     (set! max-y y)
			     (set! max-tot tot)))			
		      ;;(format #t "~%~%------------------------------------------------------~%~%")
		     )))))
    (values 'x max-x 'y max-y 'tot max-tot)))

#|

x , y => power 
243 , 16 => 31


|#








