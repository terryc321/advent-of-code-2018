
(import (chicken format)) ;; format 
(import (chicken io)) ;; io read-line
(import (chicken process)) ;; ???
;; (import (chicken documentation))
(import (chicken process-context)) ;; current-directory / change-directory
(import (chicken pretty-print)) ;; pp 

(import regex) ;; string-match 

(import bindings) ;; bind

(import srfi-69) ;; hashes

(import srfi-1) ;; member?

;;(import procedural-macros) ;; define-macro
(import (chicken syntax)) ;; macro transformers

;; (current-directory)
;; (change-directory "day17/chicken")


(define get-input
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
	(let f ((x (read-line p)))
	  (if (eof-object? x)
              '()
	      (if (zero? (string-length x))
		  (f (read-line p))
		  (cons x (f (read-line p))))))))))


(define decipher 
  (let ((regex1 (regexp "x=([0-9]+).*y=([0-9]+)[.][.]([0-9]+)"))
	(regex2 (regexp "y=([0-9]+).*x=([0-9]+)[.][.]([0-9]+)")))
    (lambda (s)
      (let ((matches-a (string-match regex1 s))
	    (matches-b (string-match regex2 s)))
	(cond
	 ((and matches-a matches-b)
	  (format #t "decipher - inconsistent cannot match both regexs surely? ")
	  (error "decipher"))
	 (matches-a (cons 'x-y-y
			  (map (lambda (r) (string->number r 10))
			       (cdr matches-a))))
	 (matches-b (cons 'y-x-x
			  (map (lambda (r) (string->number r 10))
			       (cdr matches-b))))
	 (#t (error (format #f "no match on line ~a" s))))))))


(define range
  (lambda (expr)
    (cond
     ((eq? (car expr) 'x-y-y) (apply range-x-y-y (cdr expr)))
     ((eq? (car expr) 'y-x-x) (apply range-y-x-x (cdr expr)))
     (#t (error (format #f "range : bad x-y-y or y-x-x ~a in ~a" (car expr) expr))))))

(define range-x-y-y
  (lambda args
    (bind (x y1 y2) args ;;(format #t "making range x ~a : y ~a -> y ~a ~%" x y1 y2)
	  (recur-range-x-y-y x y1 y2))))

(define range-y-x-x
  (lambda args
    (bind (y x1 x2) args ;;(format #t "making range y ~a : x1 ~a -> x2 ~a ~%" y x1 x2)
	  (recur-range-y-x-x y x1 x2))))

(define recur-range-x-y-y
  (lambda (x y1 y2)
    (cond
     ((= y1 y2) (list (list x y1)))
     (#t (cons (list x y1) (recur-range-x-y-y x (+ y1 1) y2))))))

(define recur-range-y-x-x
  (lambda (y x1 x2)
    (cond
     ((= x1 x2) (list (list x1 y)))
     (#t (cons (list x1 y) (recur-range-y-x-x y (+ x1 1) x2))))))


(define fix
  (lambda (f)    
    (apply append
	   (let ((points (map range (map (lambda (str)
					   (format #t "str : ~a~%" str)
					   (decipher str))
					 (get-input f)))))
	     (cond
	      ((member '(495 7) points equal?) (error "fix")))
	     points))))



(define input (fix "../input.txt"))
(define example (fix "../example.txt"))
(define puzzle (fix "../puzzle1.txt"))

;; =========== input example puzzle ==== appear to be a list of points =================
;; so far so good
;;
;; (find-limits input) => (404 628 1 1631)
;; (find-limits example) => (404 628 1 1631)
;;
;; =====================================================================================

;; BUG here - or in decoding of data since visually
;; min x for example is 495 to max-x 506
;; min-y 0= the sprinkler  ... y=13 
(define find-limits
    (lambda (points)
      (let ((min-x #f)(max-x #f)
	    (min-y #f)(max-y #f))
	(letrec ((recur (lambda (pts)
			  (cond
			   ((null? pts) (list min-x max-x min-y max-y))
			   (#t (let ((point (car pts)))
				 (bind (x y) point
				       (when (or (not min-x) (< x min-x)) (set! min-x x))
				       (when (or (not max-x) (> x max-x)) (set! max-x x))
				       (when (or (not min-y) (< y min-y)) (set! min-y y))
				       (when (or (not max-y) (> y max-y)) (set! max-y y))
				       (recur (cdr pts)))))))))
	  (recur points)))))




;;==========================================================================================
;; how are we going to represent the grid ?
;; sprinkler is at (500,0)
;;
;; ../example1.txt is the example problem first posed - visually verified
;; 
(define gold-input
  (call-with-input-file "../dat/input.dat"
    (lambda (p)
      (read p))))

(define gold-example
  (call-with-input-file "../dat/example.dat"
    (lambda (p)
      (read p))))

(define (integrity xs ys)
  (cond
   ((and (null? xs) (null? ys)) 'pass)
   ((and (null? xs) (not (null? ys)))
    (format #t " ys (B) has extra items ~a~%" ys))
   (#t (let ((a (car xs)))
	 (cond
	  ((member a ys equal?)
	   (integrity (filter (lambda (x) (not (equal? x a))) xs)
	              (filter (lambda (y) (not (equal? y a))) ys)))
	  (#t
	   (format #t "mismatch A has ~a but B does not ! ? ! ~%" a)
	   (integrity (cdr xs) ys)))))))


(define (check-input-integrity)
  (integrity input gold-input))

(define (check-example-integrity)
  (integrity example gold-example))

;; ========= passed integrity tests ===============
#|
but both structurally exactly the same lists ? so how could it fail integrity ?

#;629> example
((495 2) (495 3) (495 4) (495 5) (495 6) (495 7) (495 7) (496 7) (497 7) (498 7) (499 7) (500 7) (501 7) (501 3) (501 4) (501 5) (501 6) (501 7) (498 2) (498 3) (498 4) (506 1) (506 2) (498 10) (498 11) (498 12) (498 13) (504 10) (504 11) (504 12) (504 13) (498 13) (499 13) (500 13) (501 13) (502 13) (503 13) (504 13))
#;631> gold-example
((495 2) (495 3) (495 4) (495 5) (495 6) (495 7) (495 7) (496 7) (497 7) (498 7) (499 7) (500 7) (501 7) (501 3) (501 4) (501 5) (501 6) (501 7) (498 2) (498 3) (498 4) (506 1) (506 2) (498 10) (498 11) (498 12) (498 13) (504 10) (504 11) (504 12) (504 13) (498 13) (499 13) (500 13) (501 13) (502 13) (503 13) (504 13))
#;633> (equal? example gold-example)
#t
#;635> (equal? input gold-input)
#t
#;649> 
|#

;; ========= a flat list is just too slow ===========
;; lets try srfi-69 hash table

(define input-hash (make-hash-table #:test equal?))
(define example-hash (make-hash-table #:test equal?))

(define (hashify-grid g hash)
  (cond
   ((null? g) hash)
   (#t (let ((a (car g)))
	 (bind (x y) a
	       (hash-table-set! hash a #\#)
	       (hashify-grid (cdr g) hash))))))

(hashify-grid input input-hash)
(hashify-grid example example-hash)

(hash-table-set! input-hash 'limits (find-limits input))
(hash-table-set! example-hash 'limits (find-limits example))

;; dont forget sprinkler !
(hash-table-set! input-hash '(500 0) #\+)
(hash-table-set! example-hash '(500 0) #\+)


(define (show-grid hash)
  (bind (x1 x2 y1 y2)	(hash-table-ref hash 'limits)
	(let loop ((x x1)(y 0))
	  (cond
	   ((> y y2) #f)
	   ((> x x2)
	    (format #t "~%")
	    (loop x1 (+ y 1)))
	   (#t
	    (let ((ch (hash-table-ref/default hash (list x y) #f)))
	      (cond
	       ((not ch) (format #t "."))
	       ((char=? ch #\+) (format #t "+"))
	       ((char=? ch #\#) (format #t "#"))
	       ((char=? ch #\~)  (format #t "~"))
	       (#t (error "show-grid")))
	      (loop (+ x 1) y)))))))


;; water starts falling from 500 0
;; vertically down (increasing y direction)
;; until it hits a wall #

;;
;;      ~
;;      ~
;;      ~
;;      #  aha ! hit a wall  --->  search left and right 
;; case 1 : wall both sides - in which case - fill both sides up and go up
;; case 2 : wall on left but not wall on right .. waterfall to right 
;; case 3 : wall on right but not wall on left .. waterfall to left
;;
;; once in waterfall - do we ever return ?
;;


;; no need to use hash here 
(define (hash-too-far? y ylim) ;;hash x y ylim)
   (> y ylim))


(define (hash-sand? hash x y ylim)
  (cond
   ((> y ylim) #f)
   (#t (let ((ch (hash-table-ref/default hash (list x y) #f)))
	 (cond
	  ((not ch) #t)
	  ((char=? ch #\~) #f)
	  ((char=? ch #\#) #f)
	  ((char=? ch #\s) #t)	  
	  (#t #f))))))



(define (hash-empty? hash x y ylim)
  (cond
   ((> y ylim) #f)
   (#t (let ((ch (hash-table-ref/default hash (list x y) #f)))
	 (cond
	  ((not ch) #t)
	  (#t #f))))))


(define (hash-wall? hash x y ylim)
  (cond
   ((> y ylim) #f)
   (#t (let ((ch (hash-table-ref/default hash (list x y) #f)))
	 (cond
	  ((not ch) #f)
	  ((char=? ch #\#) #t)
	  (#t #f))))))

(define (hash-water! hash x y ylim)
  (cond
   ((> y ylim) #f)
   (#t (let ((ch (hash-table-ref/default hash (list x y) #f)))
	 (cond
	  ((not ch) (hash-table-set! hash (list x y) #\~))
	  ((char=? ch #\#) (error "water! cannot put water where wall is!")))))))
   

(define (hash-waterfall! hash x y ylim)
  (cond
   ((> y ylim) #f)
   (#t (let ((ch (hash-table-ref/default hash (list x y) #f)))
	 (cond
	  ((not ch) (hash-table-set! hash (list x y) #\|))
	  ((char=? ch #\#) (error "water! cannot put waterfall where wall is!")))))))
   

;; do we need more information ?
;; have a think ?
;; see what works 
;;

(define (move hash)
  (let ((limits (hash-table-ref hash 'limits))
	(counter -1))
    (bind (x1 x2 y1 ylim) limits
	  (format #t "limits were ~a ~%" limits)
	  
	  (letrec ((water! (lambda (x y) (hash-water! hash x y ylim)))
		   (wall? (lambda (x y) (hash-wall? hash x y ylim)))
		   (empty? (lambda (x y) (hash-empty? hash x y ylim)))
		   (sand? (lambda (x y) (hash-sand? hash x y ylim)))		   
		   (too-far? (lambda (x y) (hash-too-far? y ylim)))
		   (go-down   (lambda (x y)
				;; (format #t "counter ~a~%" (begin (set! counter(+ counter 1))
				;; 				 counter))
				;; (format #t "go-down ~a ~a~%" x y)
				;; (show-grid hash)
				;; (format #t "~%~%")
				(cond
				 ((too-far? x y) #f)
				 ((empty? x y)
				  (waterfall! x y)
				  (go-down x (+ y 1)))
				 ((wall? x y)
				  (go-left x (- y 1) x (- y 1)))
				 ((sand? x y)
				  (go-left (- x 1) y x (- y 1)))
				 (#t
				  (error "go down case not handled")))))				 			
		   (go-left   (lambda (x y ox oy)
				;; (format #t "counter ~a~%" (begin (set! counter(+ counter 1))
				;; 				 counter))
				;; (format #t "go-left ~a ~a ~a ~a~% " x y ox oy)
				;; (show-grid hash)
				;; (format #t "~%~%")
				
				(cond
				 ((and (empty? x y) (empty? x (+ y 1))) ;; waterfall
				  (waterfall! x y)
				  (go-down x y) ;; once we finally return
				  (go-right (+ x 1) y ox oy 'waterfall))
				 ((and (empty? x y) (empty? x (+ y 1))) ;; on water
				  (water! x y)
				  (go-left (- x 1) y ox oy)
				  )				 
				 ((wall? x y)
				  (go-right (+ x 1) y ox oy 'wall))
				 ((empty? x y) ;; floor underneath ok
				  (water! x y)
				  (go-left (- x 1) y ox oy))
				 (#t (error "go-left case not handled")))))
		   (go-right (lambda (x y ox oy hist)
				;; (format #t "counter ~a~%" (begin (set! counter(+ counter 1))
				;; 				 counter))
   			        ;; (format #t "go-right ~a ~a ~a ~a ~a~% " x y ox oy hist)
				;; (show-grid hash)
				;; (format #t "~%~%")
			       (cond
				 ((and (empty? x y) (empty? x (+ y 1))) ;; waterfall
				  (water! x y)
				  (go-down x y) ;; once we finally return - done
				  )
				 ((and (empty? x y) (empty? x (+ y 1))) ;; on water
				  (water! x y)
				  (go-right (+ x 1) y ox oy hist)
				  )				 
				 ((wall? x y)
				  (cond
				   ((eq? hist 'wall)
				    ;; (format #t "was wall both sides -> going up ~%")
				    (water! ox (- oy 1))
				    (go-left (- ox 1) (- oy 1) ox (- oy 1)))
				   ((eq? hist 'waterfall) ;; we are done 
				    #t)
				   (#t (error "go right case not handled neither wall nor waterfall"))))
				 ((empty? x y) ;; floor underneath ok
				  (water! x y)
				  (go-right (+ x 1) y ox oy hist))
				 (#t (error "go-right case not handled")))))
		   )
	       ;; start at sprinkler
	    (go-down 500 1)
	    (format #t "the amount of water excluding sprinkler is ~a~%" (count-water hash))
	    ))))



;; count water #\~ and #\+ sprinkler
(define (count-water hash)
  (let ((n 0))
    (hash-table-for-each hash (lambda (k v)
				(cond
				 ((char? v)
				  (cond
				   ((char=? v #\~) (set! n (+ n 1)))
				   ;; ((char=? v #\+) (set! n (+ n 1)))
				   )))))
    n))



;; Part One - AOC 2018 - Day 17 
;; #;681> (move input-hash)
;; limits were (404 628 6 1631) 
;; the amount of water excluding sprinkler is 280960
;; #;686> 
	  
;; BOOO answer rejected - answer is too high 		
