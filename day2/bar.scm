

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
some test cases

    abcdef contains no letters that appear exactly two or three times.
    bababc contains two a and three b, so it counts for both.
    abbcde contains two b, but no letter appears exactly three times.
    abcccd contains three c, but no letter appears exactly two times.
    aabcdd contains two a and two d, but it only counts once.
    abcdee contains two e.
    ababab contains three a and three b, but it only counts once.

read a string and find letters that appear exactly two or three times


|#

(define (count-letters s)
   (let ((alphabet (make-vector 26 0)) ;; 26 long 0 to 25 inclusive
	 (slist (string->list s))
	 (ascii-a (char->integer #\a))
	 (ascii-z (char->integer #\z)))
     (letrec ((foo (lambda (xs)
		     (cond
		      ((null? xs) #f)
		      (#t (let* ((letter (car xs))
				 (ascii (char->integer letter)))
			    (assert (and (>= ascii ascii-a) (<= ascii ascii-z)))
			    (let ((ivec (- ascii ascii-a)))
			      (vector-set! alphabet ivec (+ 1 (vector-ref alphabet ivec)))
			      (foo (cdr xs)))))))))
       (foo slist)

       (let ((nthree 0)
	     (ntwo 0))
	 (do-times k 26
		  (let ((vec-k (vector-ref alphabet k)))
		    (cond
		     ((= vec-k 2) (set! ntwo 1))
		     ((= vec-k 3) (set! nthree 1))
		     ;;((= vec-k 2) (set! ntwo (+ 1 ntwo)))
		     ;;((= vec-k 3) (set! nthree (+ 1 nthree)))
		     )))
	 (list ntwo nthree alphabet)))))

       


#|
    abcdef contains no letters that appear exactly two or three times.
    bababc contains two a and three b, so it counts for both.
    abbcde contains two b, but no letter appears exactly three times.
    abcccd contains three c, but no letter appears exactly two times.
    aabcdd contains two a and two d, but it only counts once.
    abcdee contains two e.
    ababab contains three a and three b, but it only counts once.
|#

;; (count-letters "abcdef")
;; (count-letters "bababc")

(define (get-id)
  (let ((rs (map count-letters input)))
    (let ((ntwo 0)
	  (nthree 0))
      (map (lambda (x)
	     (set! ntwo (+ ntwo (car x)))
	     (set! nthree (+ nthree (cadr x))))
	   rs)
      (list (* ntwo nthree) ntwo nthree))))

#|
#;1793> (get-id)
(16510 635 26)

only wanted to count if string has any 3 letters - all occurrences different 3 letters all count as just 1
ditto 2 letters multiple 2 letters in string all count only as 1


#;2024> (get-id)
(6474 249 26)

part 2 puzzle

find two strings that differ only by a single letter
all strings are 26 in length
|#		    

;; this should find string difference for two equal 
;; string-length 26 in length 0 to 25 inclusive index	       
(define (find-difference s1 s2)
  (letrec ((foo (lambda (n d)
		  (cond
		   ((> n 25) d)
		   ((char=? (string-ref s1 n) (string-ref s2 n))
		    (foo (+ n 1) d))
		   (#t (foo (+ n 1) (+ d 1)))))))
    (foo 0 0)))


#|
take list of strings
convert to vector
match 0 .. 1 .. n-1
match 1 .. 2 .. n-1
match 2 .. 3 .. n-1
match n-1 ,,, gone too far
|#
(define (find-closest data)
  (let* ((vec (list->vector data))
	 (lim (- (vector-length vec) 1))
	 (lim2 (- lim 1)))
    (do-for (a 0 lim)
	    (do-for (b (+ a 1) lim2)
		    (let* ((s1 (vector-ref vec a))
			   (s2 (vector-ref vec b))
			   (diff (find-difference s1 s2)))
		      (cond
		       ((= diff 1) (format #t "~a : ~a : ~a : ~a : ~a~%" a b s1 s2 
					   (fix-difference s1 s2)))))))))

(define (fix-difference s1 s2)
  (letrec ((foo (lambda (n s)
		  (cond
		   ((> n 25) (list->string (reverse s)))
		   ((char=? (string-ref s1 n) (string-ref s2 n))
		    (foo (+ n 1) (cons (string-ref s1 n) s)))
		   (#t (foo (+ n 1) s))))))
    (foo 0 '())))

  

(define (test)
  (find-difference (list-ref input 0) (list-ref input 1)))

#|
75 : 105 : indexes into input , probably 76th and 106th entry 
mxhwoglxgeauywfdkztndcvjqr : 
mxhwoglxgeauywfikztndcvjqr : 
               ^---- here is single difference
mxhwoglxgeauywfkztndcvjqr


|#



		      

		    	 
		    
