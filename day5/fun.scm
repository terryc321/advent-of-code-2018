

(import scheme)
(import (chicken pretty-print))
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

(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))
(define input #f)

(define (reset)
  (set! input (get-input))
  (set! input (string->list input)))


(reset)



(define (reactive? a b)
  (assert (char? a))
  (assert (char? b))
  (and (not (char=? a b))
       (char-ci=? a b)))

#|
if input was a vector 
use vector-refs ...

(define (scan)
  (let ((lim (vector-length input)))
  (do-for (j 0 lim)
	  (let ((c1 (vector-ref input j)))
	    (cond
	     ((char? c1)
	      (let ((k (+ j 1)))
		(cond
		 ((< k lim)
		  (let ((c2 (vector-ref input k)))
|#	   


#|
       () .... ()
       (#\a) ... (#\a)
       (#\a #\A ...
|#	  
(define flag #f)


(define (scan xs)
  (cond
   ((null? xs) xs)
   ((null? (cdr xs)) xs)
   (#t (let ((a (first xs))
	     (b (second xs)))
	 (cond
	  ((reactive? a b)
	   (set! flag #t)
	   (scan (cdr (cdr xs))))
	  (#t  (cons a (scan (cdr xs)))))))))


(define (repeat xs)
  (set! flag #f)
  (let ((ys (scan xs)))
    (cond
     (flag (repeat ys))
     (#t ys))))


(define (part-1)
  (reset)
  (repeat input))


#|
#;1545> (part-1)
(#\A #\p #\T #\D #\I #\L #\j #\m #\Y #\p #\A #\X #\Z #\K #\r #\l #\e #\v #\L #\I #\Z #\W #\P #\O #\O #\e #\z #\a #\C #\y #\B #\M #\q #\e #\A #\u #\R #\s #\h #\r #\S #\t #\V #\z #\r #\N #\x #\T #\G #\i #\O #\J #\p #\w #\y #\x #\C #\f #\n #\p #\q #\v #\e #\D #\j #\B #\L #\t #\D #\L #\K #\o #\j #\y #\W #\g #\D #\n #\z #\x #\x #\a #\n #\y #\H #\J #\l #\t #\c #\y #\m #\s #\W #\m #\E #\N #\u #\k #\g #\W #\q #\p #\W #\K #\s #\n #\J #\H #\P #\V #\k #\n #\C #\z #\X #\q #\W #\J #\n #\Q #\j #\f #\G #\H #\d #\Q #\a #\x #\V #\q #\n #\H #\k #\Y #\f #\t #\N #\y #\H #\g #\P #\w #\t #\Y #\f #\h #\b #\b #\u #\G #\q #\S #\j #\a #\J #\r #\P #\g #\Y #\K #\j #\n #\Y #\x #\s #\v #\u #\l #\q #\P #\G #\T #\p #\m #\Y #\V #\V #\g #\o #\p #\W #\J #\G #\D #\D #\D #\S #\X #\N #\K #\Y #\l #\I #\e #\e #\h #\z #\y #\C #\W #\Y #\E #\D #\a #\v #\h #\v #\w #\K #\h #\O #\C #\Q #\Y #\f #\P #\q #\v #\F #\l #\A #\Y #\V #\O #\F #\T #\m #\n #\g #\Z #\Y #\W #\E #\C #\g #\d #\B #\Q #\K #\w #\w #\I #\e #\N #\v #\f #\d #\b #\M #\r #\m #\a #\u #\f #\h #\b #\H #\S #\A #\K #\i #\X #\O #\X #\X #\C #\r #\U #\n #\S #\i #\S #\V #\z #\U #\C #\e #\D #\t #\A #\T #\G #\q #\y #\L #\y #\N #\Q #\b #\U #\K #\g #\g #\W #\X #\z #\E #\U #\r #\U #\r #\N #\J #\s #\z #\m #\a #\x #\z #\Q #\J #\F #\R #\w #\A #\u #\k #\W #\T #\B #\o #\d #\H #\D #\w #\u #\J #\q #\q #\m #\F #\h #\Z #\Y #\k #\U #\a #\X #\U #\r #\J #\U #\U #\v #\U #\P #\i #\Z #\k #\J #\y #\S #\M #\S #\Q #\f #\W #\K #\h #\w #\U #\E #\H #\r #\N #\K #\Y #\T #\I #\n #\U #\p #\G #\B #\D #\f #\t #\N #\R #\K #\n #\w #\T #\K #\n #\k #\L #\U #\i #\X #\f #\l #\q #\E #\V #\I #\a #\l #\M #\f #\l #\X #\i #\Q #\i #\G #\p #\E #\T #\g #\E #\Y #\f #\L #\d #\m #\j #\P #\h #\b #\W #\K #\z #\r #\Y #\U #\B #\O #\S #\C #\G #\l #\U #\Y #\o #\W #\Y #\q #\L #\Q #\e #\A #\y #\V #\w #\X #\J #\w #\y #\g #\p #\s #\v #\U #\g #\r #\Y #\V #\h #\F #\K #\l #\w #\U #\k #\u #\q #\r #\X #\Z #\l #\u #\k #\a #\M #\s #\j #\b #\X #\e #\e #\r #\W #\j #\s #\V #\j #\n #\i #\g #\X #\n #\W #\G #\E #\D #\f #\Q #\b #\T #\o #\Z #\X #\b #\y #\F #\j #\S #\t #\W #\O #\H...
#;1639> ,t (length (part-1))
8.32s CPU time, 0.195s GC time (major), 1532074/255351 mutations (total/tracked), 68/102625 GCs (major/minor), maximum live heap: 3.25 MiB
10888

accepted answer

|#

#|
for part 2 
take input
filter out a A
filter out b B
filter out c C
filter out d D 
find letter than leaves shortest string
|#
(define (part-2)
  (reset)
  (let ((res '()))
  (do-list (i (iota 26))
	   (let ((c1 (integer->char (+ i 97)))
		 (c2 (integer->char (+ i 65))))
	     (let ((ys (filter (lambda (c) (not (or (char=? c c2)
						    (char=? c c1))))
			       input)))
	       (let* ((out (repeat ys))
		      (len (length out)))
		 (format #t "letter ~a ~a : resultant ~a ~%" c1 c2 len)
		 (set! res (cons `((len ,len)(char ,c1 ,c2)) res))
		 ))))
  res))


#|

#;2444> (part-2)
letter a A : resultant 10450 
letter b B : resultant 10456 
letter c C : resultant 10502 
letter d D : resultant 6952 
letter e E : resultant 10436 
letter f F : resultant 10452 
letter g G : resultant 10438 
letter h H : resultant 10472 
letter i I : resultant 10456 
letter j J : resultant 10432 
letter k K : resultant 10372 
letter l L : resultant 10436 
letter m M : resultant 10482 
letter n N : resultant 10466 
letter o O : resultant 10490 
letter p P : resultant 10462 
letter q Q : resultant 10456 
letter r R : resultant 10478 
letter s S : resultant 10466 
letter t T : resultant 10420 
letter u U : resultant 10464 
letter v V : resultant 10454 
letter w W : resultant 10456 
letter x X : resultant 10452 
letter y Y : resultant 10442 
letter z Z : resultant 10422 

(((len 10422) (char #\z #\Z)) ((len 10442) (char #\y #\Y)) ((len
10452) (char #\x #\X)) ((len 10456) (char #\w #\W)) ((len 10454) (char
#\v #\V)) ((len 10464) (char #\u #\U)) ((len 10420) (char #\t #\T))
((len 10466) (char #\s #\S)) ((len 10478) (char #\r #\R)) ((len 10456)
(char #\q #\Q)) ((len 10462) (char #\p #\P)) ((len 10490) (char #\o
#\O)) ((len 10466) (char #\n #\N)) ((len 10482) (char #\m #\M)) ((len
10436) (char #\l #\L)) ((len 10372) (char #\k #\K)) ((len 10432) (char
#\j #\J)) ((len 10456) (char #\i #\I)) ((len 10472) (char #\h #\H))
((len 10438) (char #\g #\G)) ((len 10452) (char #\f #\F)) ((len 10436)
(char #\e #\E)) ((len 6952) (char #\d #\D)) ((len 10502) (char #\c
#\C)) ((len 10456) (char #\b #\B)) ((len 10450) (char #\a #\A)))
#;2524> 

lowest achieve 
letter d D : resultant 6952 

6952

|#

  


