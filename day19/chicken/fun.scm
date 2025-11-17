
#|

advent of code day 19 

ip=0 [0, 0, 0, 0, 0, 0] seti 5 0 1 [0, 5, 0, 0, 0, 0]
ip=1 [1, 5, 0, 0, 0, 0] seti 6 0 2 [1, 5, 6, 0, 0, 0]
ip=2 [2, 5, 6, 0, 0, 0] addi 0 1 0 [3, 5, 6, 0, 0, 0]
ip=4 [4, 5, 6, 0, 0, 0] setr 1 0 0 [5, 5, 6, 0, 0, 0]
ip=6 [6, 5, 6, 0, 0, 0] seti 9 0 5 [6, 5, 6, 0, 0, 9]



|#

(import srfi-1)
(import srfi-12)
(import (chicken format))
(import (chicken pretty-print))



(define (make-state) (make-vector 6 0))

;; zero based ok

(define a (make-state))
(define b (make-state))

(vector-set! a 1 -1)
a
b

(define (example)
  `((ip 0) ;; we ignore this
    (seti 5 0 1)
    (seti 6 0 2)
    (addi 0 1 0)
    (addr 1 2 3)
    (setr 1 0 0)
    (seti 8 0 4)
    (seti 9 0 5)))

(define (input)
  `(
    (ip 2) ;; we ignore this 
    (addi 2 16 2)
    (seti 1 0 1)
    (seti 1 3 3)
    (mulr 1 3 5)
    (eqrr 5 4 5)
    (addr 5 2 2)
    (addi 2 1 2)
    (addr 1 0 0)
    (addi 3 1 3)
    (gtrr 3 4 5)
    (addr 2 5 2)
    (seti 2 6 2)
    (addi 1 1 1)
    (gtrr 1 4 5)
    (addr 5 2 2)
    (seti 1 1 2)
    (mulr 2 2 2)
    (addi 4 2 4)
    (mulr 4 4 4)
    (mulr 2 4 4)
    (muli 4 11 4)
    (addi 5 6 5)
    (mulr 5 2 5)
    (addi 5 19 5)
    (addr 4 5 4)
    (addr 2 0 2)
    (seti 0 7 2)
    (setr 2 6 5)
    (mulr 5 2 5)
    (addr 2 5 5)
    (mulr 2 5 5)
    (muli 5 14 5)
    (mulr 5 2 5)
    (addr 4 5 4)
    (seti 0 7 0)
    (seti 0 3 2)))




;; feed it with a program a list of instructions
;; place this into an array or vector
;; start at index 0 , first element of vector
;; bounds check if goes outside this - program halts
;; retrieve first instruction 

;; ==> really look into macros in chicken scheme
;; ==> define-syntax syntax-rules ?? hygiene

;; ;; p program
;; ;; as a state 
;; ;; pv program vector
;; (define l->v list->vector)


;; (define-syntax def
;;   (syntax-rules ()
;;     ((def a n ...) (define a (begin n ...)))
;;     ((def (a ...) ...) (define a (lambda (...) ...)))))

;; ;; (defmacro def (args . body)
;; ;;   `(define ,@args ,@body))

;; ;; st -> state
;; ;; sim -> simulate
;; ;; p   -> the program (a list)
;; ;; pv  -> the program vector (an array

;; (def sim (p)
;;   (let ((as (st))
;; 	(pv (l->v p))

;; seti set immediate a b c   a->c ignore b 

;; first #ip 0 is not an instruction ? 
;; anyways
;;

(define (sim-real p *ip-bound* state)
  (let* ((pvec (list->vector p))	 
	 (lasti (- (vector-length pvec) 1))
	 (exit #f)
	 (ip 0)
	 (last-state-index 5) ;; 6 item array zero index 0 1 2 - 3 4 5
	 (*ip-bound* 0))
    (let loop ()
      (letrec ((leave (lambda (fn) (if (fn) (exit #t) #f)))
	       (before (lambda () (vector-set! state *ip-bound* ip)))
	       (after (lambda ()
			(leave (lambda () (or (< *ip-bound* 0)(> *ip-bound* last-state-index))))
			(set! ip (vector-ref state *ip-bound*))
			(set! ip (+ ip 1))))
	       (during (lambda ()
			 (leave (lambda () (or (< ip 0)(> ip lasti))))			 
			 (let ((ins (vector-ref pvec ip)))
			   ;; (format #t " ~a " ins)
			   (if 
			    (eq? (first ins) 'ip)
			    (begin
			      (set! *ip-bound* (second ins)))
			    (begin
			      (let ((in (first ins))
				    (n1 (second ins))
				    (n2 (third ins))
				    (n3 (fourth ins)))
				(cond
				 ((eq? in 'seti) ;; valA -> regC
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))			 
				  (vector-set! state n3 n1))
				 ((eq? in 'addi) ;; regA + valueB -> regC
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
		 		  (vector-set! state n3 (+ (vector-ref state n1) n2)))
				 ((eq? in 'gtrr) ;; 
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
				  (leave (lambda () (or (< n2 0)(> n2 last-state-index))))
		 		  (vector-set! state n3 (if (> (vector-ref state n1) (vector-ref state n2)) 1 0)))
				 ((eq? in 'muli) ;; 
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
		 		  (vector-set! state n3 (* (vector-ref state n1) n2)))
				 ((eq? in 'addr) ;; 
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
				  (leave (lambda () (or (< n2 0)(> n2 last-state-index))))
		 		  (vector-set! state n3 (+ (vector-ref state n1) (vector-ref state n2))))
				 ((eq? in 'eqrr) ;; 
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
				  (leave (lambda () (or (< n2 0)(> n2 last-state-index))))
		 		  (vector-set! state n3 (if (= (vector-ref state n1) (vector-ref state n2)) 1 0)))
				 ((eq? in 'mulr) ;; regc <- rega * regb
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
				  (leave (lambda () (or (< n2 0)(> n2 last-state-index))))
		 		  (vector-set! state n3 (* (vector-ref state n1)
							   (vector-ref state n2))))
				 ((eq? in 'setr) ;; regA -> regC
				  (leave (lambda () (or (< n3 0)(> n3 last-state-index))))
				  (leave (lambda () (or (< n1 0)(> n1 last-state-index))))
				  (vector-set! state n3 (vector-ref state n1)))
				 (#t (format #t "never understood ~a ~%" ins)
				     (exit #t)
				     )))))))))
	(call/cc (lambda (escape)
		   (set! exit escape)
		   ;; (format #t "state => ~a " state)
		   (before)
		   (during)
		   (after)
		   ;; (format #t "state => ~a ~%" state)		   
		   (loop)))))))

	


(define (run)    
  (let ((state (make-state)))
    (sim-real (cdr (example)) 0 state)))



(define (run2)
  (let ((state (make-state)))
    (call/cc (lambda (k)
	       (with-exception-handler (lambda (x)
					 (let ((out (vector-ref state 0)))
					   (format #t "~%the value left in register 0 is : [ ~%a ] ~%" out)
					   (k '())))
				       (lambda () 
					 (sim-real (cdr (input)) 2 state)))))))


	      
(run2)

     
