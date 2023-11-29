
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
    ;; for each child do - process child
    (do-for (c 0 n-child)
	    (process-child))
    ;; read all meta-data
    (do-for (x 0 n-meta)
	    (let ((val (vector-ref input (+ vi x))))
	      (set! sum (+ sum val))
	      (format #t "meta ~a : ~a ~%" x val)))
    (set! vi (+ vi n-meta))))



;; (define (foo xs)
;;   (let* ((dat (list->vector xs))
;; 	 (lim (vector-length dat)))
;;     (letrec ((process (lambda ()

(define (part-1)
  (set! sum 0)
  (set! vi 0)  
  (process-child)
  (format #t "sum = ~a ~%" sum))


#|

sum = 48496 
vi
18855
#;1194> (vector-length input)
18855
can see vector index vi has read all data now sitting off end of vector


|#


