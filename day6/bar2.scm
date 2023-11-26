#|
second attempt at day 6 
fresh start ...

|#

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


(define (manhatten x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))


;; manhatten of square vis input
(define (sum-man-square x y cords)
  (let ((sum 0))   
    (do-list (cid cords)
	     (match cid
	       ((x2 y2)
		(let ((dist (manhatten x y x2 y2)))
		  (set! sum (+ sum dist))))))
    sum))


(define (brute x1 y1 x2 y2 cords)
  (let ((tot 0)
	(critical-dist 10000))
    (assert (> x2 x1))
    (assert (> y2 y1))
    (let* ((dx (- x2 x1))
	   (dy (- y2 y1))
	   (area (* dx dy)))
      (format #t "computing (~a,~a) -> (~a,~a) : total area ~a ~%" x1 y1 x2 y2 area))
    (do-for (x x1 x2)
	    (do-for (y y1 y2)
		    (let ((m (sum-man-square x y cords)))
		      (cond
		       ((< m critical-dist) (set! tot (1+ tot)))))))
    (format #t "tot ~a~%" tot)
    (brute (10- x1) (10- y1) (10+ x2) (10+ y2) cords)))



(define (part-2)
  (let* ((lo-x (apply min (map first input)))
	 (hi-x (apply max (map first input)))
	 (lo-y (apply min (map second input)))
	 (hi-y (apply max (map second input)))
	 (prev #f))
    (brute lo-x lo-y hi-x hi-y input)))


(part-2)


#|
computing (40,42) -> (359,356) : total area 100166 
tot 40134
computing (30,32) -> (369,366) : total area 113226 
tot 40134
computing (20,22) -> (379,376) : total area 127086 
tot 40134
computing (10,12) -> (389,386) : total area 141746 
tot 40134
computing (0,2) -> (399,396) : total area 157206 
tot 40134
computing (-10,-8) -> (409,406) : total area 173466 
tot 40134
computing (-20,-18) -> (419,416) : total area 190526 
tot 40134
computing (-30,-28) -> (429,426) : total area 208386 
tot 40134
computing (-40,-38) -> (439,436) : total area 227046 
tot 40134
computing (-50,-48) -> (449,446) : total area 246506 
tot 40134
computing (-60,-58) -> (459,456) : total area 266766 
tot 40134
computing (-70,-68) -> (469,466) : total area 287826 

40134 then ??


|#
