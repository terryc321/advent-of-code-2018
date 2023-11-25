

(import scheme)
(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "../day2")
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

(define input (call-with-input-file "input"
		(lambda (port)
		  (read port))))

#|

square inches of fabric
atleast 1000 x 1000 of fabric available 

whenever hash-table has hit on x y with already hit - that square gets added to hit-count total

|#

(define hash #f)
(define hit-count 0)

(define (reset)
  (set! hash (make-hash-table))
  (set! hit-count 0)
)

;; uses global hash
;; kind of redundant no need to store what id's actually hit the x y 
(define (hit x y id)
  (let ((val (hash-table-ref/default hash (list x y) #f)))
    (cond
     (val (when (= (length val) 1) 
	    (set! hit-count (+ 1 hit-count)))
	  (hash-table-set! hash (list x y) (cons id val)))
     (#t (hash-table-set! hash (list x y) (list id))))))


(define (fuz dat)
  (reset)
  (do-list (x dat)
	   (match x 
	     ((id offx offy dx dy) 
	      (do-for (x 0 dx)
		      (do-for (y 0 dy)
			      (let ((hx (+ x offx))
				    (hy (+ y offy)))
				;;(format #t "hit ~a ~a with id ~a ~%" hx hy id)
			      (hit hx hy id)))))
	     ( _ (error "fuz"))))
  (format #t "hit count = ~a ~%" hit-count)
  hit-count)

#|
hit-count represents number of squares that have one or more overlaps


#;51> (fuz input)
hit count = 116140 
116140

|#


