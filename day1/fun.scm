

(import scheme)
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

(define input (call-with-input-file "input"
		(lambda (port)
		  (read port))))


;; 
;;(iter)


input;; ------------ puzzle ---------------------------
(define (part-1)
  (fold + 0 input))	

#|
curiously says there is no repeat frequency when run through values
|#
(define (part-2 dat)
  (let ((hash (make-hash-table))
	(freq 0))
    (hash-table-set! hash 0 #t)
    (letrec ((foo (lambda (xs)
		    (cond
		     ((null? xs) (foo dat)) ;; repeat the list again
		     (#t (let ((a (car xs)))
			   (set! freq (+ freq a))
			   (format #t "~a : ~a ~%" a freq)
			   (cond			    
			    ((hash-table-ref/default hash freq #f) freq)
			    (#t (hash-table-set! hash freq #t)
				(foo (cdr xs))))))))))
      (foo dat))))


(define (test-cases)
  (= 0 (part-2 '( +1 -1)))                ;; first reaches 0 twice.
  (= 10 (part-2 '(  +3  +3  +4  -2  -4))) ;; first reaches 10 twice.
  (= 5 (part-2 '(  -6  +3  +8  +5  -6)))   ;;  first reaches 5 twice.
  (= 14 (part-2 '(  +7  +7  -2  -7  -4))) ;; first reaches 14 twice.
)



(define (sik)
  (let ((hash (make-hash-table))
	(res '())
	(freq 0))
    (hash-table-set! hash 0 #t)
    (letrec ((foo (lambda (xs)
		    (cond
		     ((null? xs) 'no-repeat)
		     (#t (let ((a (car xs)))
			   (set! freq (+ freq a))
			   (set! res (cons freq res))
			   (format #t "~a : ~a ~%" a freq)
			   (cond			    
			    ((hash-table-ref/default hash freq #f) freq)
			    (#t (hash-table-set! hash freq #t)
				(foo (cdr xs))))))))))
      (foo input)
      (reverse res))))

(define (sak xs)
  (cond
   ((null? xs) 'no-repeat-x2)
   (#t (cond
	((member (car xs) (cdr xs)) (car xs))
	(#t (sak (cdr xs)))))))


(define (debug)
  (sak (sik)))


(define (check-hash)
  (let ((hash (make-hash-table)))
    (hash-table-set! hash 1 #t)
    (hash-table-set! hash 2 #t)
    (hash-table-set! hash 3 #t)
    (format #t "in hash ? ~a ~%" (hash-table-ref/default hash 1 #f))
    (format #t "in hash ? ~a ~%" (hash-table-ref/default hash 2 #f))
    (format #t "in hash ? ~a ~%" (hash-table-ref/default hash 3 #f))
    (format #t "in hash ? ~a ~%" (hash-table-ref/default hash 4 #f))))


    


;; ------------ answers ---------------------------
#|
 t (part-1)


 t (part-2)

|#
;; ------------ test cases -------------

(define (test-case)
  ;; (assert (equal? 6 (rec-count-safe "..^^." 3)))
  ;; (assert (equal? 38 (rec-count-safe ".^^.^.^^^^" 10)))
  #t
  )



#|
results

part-1
427

part-2
341

bug in ccode was list given is repeated over and over until a repeated frequency pops up


|#



		    
		    
		    
		      
		    	 
		    
