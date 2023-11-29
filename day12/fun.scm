
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

(define input (get-input "input"))
;;(define input2 (get-input "input2"))
;;(define input 7857)

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)

#|

# means has plant 
. means no plant

horizontal matching game 

|#

(define input-state (car input))
(define input-rules (cdr input))

(define example-state "#..#.#..##......###...###")
(define example-rules 
  '(("...##" => "#")
    ("..#.." => "#")
    (".#..." => "#")
    (".#.#." => "#")
    (".#.##" => "#")
    (".##.." => "#")
    (".####" => "#")
    ("#.#.#" => "#")
    ("#.###" => "#")
    ("##.#." => "#")
    ("##.##" => "#")
    ("###.." => "#")
    ("###.#" => "#")
    ("####." => "#")
    ))

;; takes in string as initial state , start string index 0
;; returns a procedure takes one arg - integer - returns if there is a plant there or not
(define (state->hash s)
  (let* ((slen (string-length s))
	 (res (make-hash-table)))
    (do-for (i 0 slen)
	    (let ((ch (string-ref s i)))
	      (cond
	       ((char=? #\. ch) (hash-table-set! res i #f))
	       ((char=? #\# ch) (hash-table-set! res i #t)))))
    (hash-table-set! res 'lo 0)
    (hash-table-set! res 'hi slen)    
    (lambda (k)
      (let ((lookup (hash-table-ref/default res k #f)))
	lookup))))


#|
 side effect res hash table on index i 
 rule "XXXX" four letter string
|# 
(define (apply-rules res i h rules)
  (let ((applied 0))
    (do-list (rule rules) ;; ("XXXX" => :sone ".")
	     (let ((sfive (first rule))
		   (sone (third rule)))
	       ;;(format #t "sfive = [~a] : sone = [~a]~%" sfive sone)
	       (assert (string? sfive))
	       (assert (string? sone))	   
	       (assert (= 5 (string-length sfive)))	   
	       (assert (= 1 (string-length sone)))	   
	       (let ((p1 (true-or-false-plant (string-ref sfive 0)))
		     (p2 (true-or-false-plant (string-ref sfive 1)))
		     (p3 (true-or-false-plant (string-ref sfive 2)))
		     (p4 (true-or-false-plant (string-ref sfive 3)))
		     (p5 (true-or-false-plant (string-ref sfive 4)))
		     (r1 (true-or-false-plant (string-ref sone 0)))
		     (g1 (h (- i 2)))
		     (g2 (h (- i 1)))
		     (g3 (h i))
		     (g4 (h (+ i 1)))
		     (g5 (h (+ i 2)))
		     )
		 (when
		     (and (eq? p1 g1) 
			  (eq? p2 g2) 
			  (eq? p3 g3) 
			  (eq? p4 g4)
			  (eq? p5 g5)
			  )
		   (cond
		    (r1
		     (hash-table-set! res i #t))
		    (#t
		     (hash-table-set! res i #f)))))))))





;; compute next generation given hash h
(define (next-gen h rules)
  (let ((lo (- (h 'lo) 4))
	(hi (+ (h 'hi) 4))
	(res (make-hash-table)))
    (hash-table-set! res 'lo (- lo 4))
    (hash-table-set! res 'hi (+ hi 4))
    ;; apply the rules provided
    (do-for (i lo hi)
	    (apply-rules res i h rules))
    (lambda (k)
      (let ((lookup (hash-table-ref/default res k #f)))
	lookup))))


(define (show-gen n h)
  (let ((lo (or -4 (h 'lo)))
	(hi (or 40 (h 'hi))))
    (format #t "~a : " n)
    (do-for (i lo (+ hi 1))
	    (let ((val (h i)))
	      (cond
	       (val (format #t "#"))
	       (#t (format #t ".")))))
    (format #t "~%")))

;;
(define (true-or-false-plant ch)
  (cond
   ((char=? ch #\#) #t)
   (#t #f)))
  

;;
(define (sum-up h)
  (let ((lo (h 'lo))
	(hi (h 'hi))
	(sum 0))
    (do-for (i lo hi)
	    (let ((val (h i)))
	      (cond
	       (val (set! sum (+ sum i))))))
    sum))

;;
(define (iter-sol state rules last-gen)
  (define (helper)
    (let ((cur (state->hash state))
	  (prev #f)
	  (iter 1))
      (show-gen 0 cur)
      (do-while (< iter (1+ last-gen))
		(set! prev cur)
		(set! cur (next-gen prev rules))
		(show-gen iter cur)	      
		(set! iter (1+ iter))
		)
      (sum-up cur)))
  (helper))
  

(define (example)
  (let ((last-gen 20))
    (iter-sol example-state example-rules last-gen)))


(define (part-1)
  (let ((last-gen 20))
    (iter-sol input-state input-rules last-gen)))


#|
#;5834> (part-1)
0 : ....####..##.##..##..#..###..#....#.######..#
1 : ...##.#....#..#...#.#.#.#...#.#...#.....#...#
2 : ..#.##.#....##.##.#.#.#..##.#..##..#.....##..
3 : ..#..##.#..#.#..###.#..#..##.#..#.#.#...#.#.#
4 : ...#..##.###..#.#..#.##.#..##.###.#..##.#.#.#
5 : ....#..#.....##..###..##.#..#....#.#..###.#..
6 : .....##.#...#.#..#.....##.##.#...#..#.#..#.#.
7 : ....#.##.##.#..##.#...#.#..##.##..###..###..#
8 : ....#..#..##.#..##.##.#..#..#..#..#....#...#.
9 : .....##.#..##.#..#..##.##.##.##.##.#....##.#.
10 : ....#.##.#..##.##.#..#..#..#..#..##.#..#.##.#
11 : ....#..##.#..#..##.##.##.##.##.#..##.###..##.
12 : .....#..##.##.#..#..#..#..#..##.#..#.......#.
13 : ......#..#..##.##.##.##.##.#..##.##.#.......#
14 : .......##.#..#..#..#..#..##.#..#..##.#.......
15 : ......#.##.##.##.##.##.#..##.##.#..##.#.....#
16 : ......#..#..#..#..#..##.#..#..##.#..##.#....#
17 : .......##.##.##.##.#..##.##.#..##.#..##.#....
18 : ......#.#..#..#..##.#..#..##.#..##.#..##.#..#
19 : ......#..##.##.#..##.##.#..##.#..##.#..##.###
20 : .......#..#..##.#..#..##.#..##.#..##.#..#....

3230


|#
  
  
  



       
	       
