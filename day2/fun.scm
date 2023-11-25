

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

(count-letters "abcdef")
(count-letters "bababc")

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


|#
	     
		    
		    
		    
		      
		    	 
		    
