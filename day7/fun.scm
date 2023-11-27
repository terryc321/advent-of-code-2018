
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

;; ---------------------------------------------------------------------


;;"Step R must be finished before step Y can begin."

;;(string-match precomp "Step R must be finished before step Y can begin.")
;;(string-match-positions precomp "[a]")

;; dependencies
(define (deps)
  (let ((res '()))
    (define precomp (regexp "Step ([A-Z]) must be finished before step ([A-Z]) can begin."))  
    (do-list (s input)
	     (let ((chars (cdr (string-match precomp s))))	     
	       (assert (= (length chars) 2))
	       (set! res (cons (list 
				(string-ref (second chars) 0)
				(string-ref (first chars) 0))
			       res))))
    (reverse res)))


(define (alphabet)
  (map (lambda (x)
	 (integer->char (+ (char->integer #\A) x)))
       (iota 26)))





#|
dependency 
step R must be finished before Y can begin ...
R <-- Y   meaning Y depends on R having been completed

find all things a letter depends on 

|#
(define (fiz dat alphabet)
  (let ((res '()))
    (map (lambda (ch) 
	   (cons ch (map second (filter (lambda (pair)
					  (char=? (first pair) ch)) 
					dat))))
	 alphabet)))

#|

(fiz)

((#\A #\C #\N #\F #\O)
(#\B #\F #\W #\N)
(#\C)
(#\D #\L #\T #\K #\H #\Q #\I #\X #\Z #\V #\G #\U)
(#\E #\O #\Y #\G #\D #\U #\I #\Q #\Z #\W #\P #\N)
(#\F)
(#\G #\J #\N #\L #\H #\V #\M #\O)
(#\H #\O)
(#\I #\W #\V #\X #\Q #\L #\Z #\S #\G)
(#\J #\B #\P #\F #\O #\C #\W #\H)
(#\K #\R #\F #\N)
(#\L #\C #\N)
(#\M #\F)
(#\N)
(#\O #\F #\C #\N)
(#\P #\K #\N #\O #\H)
(#\Q #\U #\S #\J #\Y #\Z)
(#\R)
(#\S #\B #\P #\K #\W)
(#\T #\N)
(#\U #\Z #\P #\N #\A #\T #\K #\O #\V)
(#\V #\A #\H #\Z)
(#\W #\M)
(#\X #\S #\J #\Q #\Y #\Z)
(#\Y #\R #\L #\P #\F)
(#\Z #\M #\J #\P #\A #\N))

C F N R - can all be done since not dependent on other tasks

process list in order , whenever i succeed in discovering a letter than can be computed
i jump back up to top of the list
since only 26 items , not a long task 

|#



(define (work xs completed n-complete)
  (cond
   ((= n-complete 26) (reverse completed))
   (#t
    (call/cc (lambda (escape)
	       (do-list (x xs)
			(match x
			  ((key . deps) 
			   ;;(format #t "key = ~a : deps ~a ~%" key deps)
			   (cond
			    ((member key completed) #f)
			    (#t (call/cc (lambda (esc)
					   (do-list (d deps)
						    (cond
						     ((member d completed) #f)
						     (#t (esc #f))))
					   (format #t "going to complete task ~A ~%" key)
					   (escape (work xs (cons key completed) (+ 1 n-complete))))))))
			  (_ (error "no-match-work")))))))))




(define (example)
  (let ((deps (fiz '( (#\A #\C) (#\B #\A) (#\D #\A) (#\E #\B) (#\E #\D) (#\E #\F) (#\F #\C))
		   '( #\A #\B #\C #\D #\E #\F )))
	(completed '())
	(n-complete 0))
    (format #t "deps = ~a ~%" deps)
    (work deps completed n-complete)))

(define (part-1)
  (let ((completed '())
	(n-complete 0))
    (work (fiz (deps) (alphabet)) completed n-complete)))


#|
(test2)
#;2861> (test)
going to complete task C 
going to complete task F 
going to complete task M 
going to complete task N 
going to complete task L 
going to complete task O 
going to complete task A 
going to complete task H 
going to complete task R 
going to complete task K 
going to complete task P 
going to complete task T 
going to complete task W 
going to complete task B 
going to complete task J 
going to complete task S 
going to complete task Y 
going to complete task Z 
going to complete task V 
going to complete task G 
going to complete task U 
going to complete task Q 
going to complete task X 
going to complete task I 
going to complete task D 
going to complete task E 
(#\C #\F #\M #\N #\L #\O #\A #\H #\R #\K #\P #\T #\W #\B #\J #\S #\Y #\Z #\V #\G #\U #\Q #\X #\I #\D #\E)

(map (lambda (x) (format #t "~a" x)) '(#\C #\F #\M #\N #\L #\O #\A #\H #\R #\K #\P #\T #\W #\B #\J #\S #\Y #\Z #\V #\G #\U #\Q #\X #\I #\D #\E))

CFMNLOAHRKPTWBJSYZVGUQXIDE

|#


























	   
