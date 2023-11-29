
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
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))


(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

#|

header two numbers

1 - number of child nodes - 0 or more 
2 - number of meta data entries - 1 or more

read header then any child nodes and their meta data then finally parent meta data entries


|#

;;(define input '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(define vi 0)
(define vlim 0)
(define sum 0)

(set! input (list->vector input))
(set! vlim (vector-length input))

(define (process-child)
  (let ((n-child (vector-ref input vi))
	(n-meta (vector-ref input (+ vi 1))))
    ;; advance 2
    (set! vi (+ vi 2))    
    ;; depending if node has child nodes
    (cond
     ((= n-child 0)
      ;; read all meta-data
      (let ((sum 0))
	(do-for (x 0 n-meta)
		(let ((val (vector-ref input (+ vi x))))
		  (set! sum (+ sum val))
		  (format #t "meta ~a : ~a ~%" x val)))
	(set! vi (+ vi n-meta))
	sum))
     (#t 
      ;; 
      (let ((xs '()))
	;; for each child do - process child
	(do-for (c 0 n-child)
		(set! xs (cons (process-child) xs)))
	(set! xs (list->vector (cons 0 (reverse xs))))
	;; read all meta-data
	(let ((sum 0))	
	  (do-for (x 0 n-meta)
		  (let ((index (vector-ref input (+ vi x))))
		    (cond
		     ((and (>= index 1) (< index (vector-length xs)))
		      (let ((val (vector-ref xs index)))
			(set! sum (+ sum val))
			(format #t "meta index ~a : ~a ~%" index val))))))
	  (set! vi (+ vi n-meta))
	  sum))))))
    


(define (part-2)
  (set! vi 0)  
  (let ((sum (process-child)))
    (format #t "sum = ~a ~%" sum)
    sum))

  


#|

sum = 48496 
vi
18855
#;1194> (vector-length input)
18855
can see vector index vi has read all data now sitting off end of vector


sum = 32850 


|#


