
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
(#\C #\F #\M #\N #\L #\O #\A #\H #\R #\K #\P #\T #\W #\B #\J #\S #\Y #\Z #\V #\G #\U #\Q #\X #\I #\D #\E)

(map (lambda (x) (format #t "~a" x)) '(#\C #\F #\M #\N #\L #\O #\A #\H #\R #\K #\P #\T #\W #\B #\J #\S #\Y #\Z #\V #\G #\U #\Q #\X #\I #\D #\E))

CFMNLOAHRKPTWBJSYZVGUQXIDE

|#

;; the clock
(define clock 0)

(define (tick) 
  (set! clock (+ clock 1)))

(define (finish-time t ch)
  (+  60
      t 
      1
     (- (char->integer ch) (char->integer #\A))))

(define completed-tasks '())

(define assigned-tasks '())

(define events '())

(define task-list #f)

(define (reset)
  (set! task-list (fiz (deps) (alphabet)))
  (set! events '())
  (set! clock 0)
  (set! completed-tasks '())
  (set! assigned-tasks '()))


;; if work available - delivers task and puts task into assigned-tasks
(define (find-work task-list)
  ;;(format #t "finding work ~%")
  (call/cc (lambda (escape)
	     (do-list (task task-list)
		      (match task
			  ((key . deps) 
			   ;;(format #t "key = ~a : deps ~a ~%" key deps)
			   (cond
			    ((member key completed-tasks) #f) ;; already completed
			    ((member key assigned-tasks) #f) ;; already assigned
			    (#t (call/cc (lambda (esc)
					   (do-list (d deps)
						    (cond
						     ((member d completed-tasks) #f)
						     (#t (esc #f))))
					   ;; all dependencies completed
					   ;;(format #t "going to complete task ~A ~%" key)
					   (set! assigned-tasks (cons key assigned-tasks))
					   (escape key))))))
			  (_ (error "no-match-work")))))))


	     


#|

tell worker to find-work
is worker already assigned a task ?
if no current-task then 

|#


(define (make-worker task-list id)
  (let ((current-task-expires 0)
	(current-task #f))

    (define (work-completed? )
      (cond
       ((and current-task (= clock current-task-expires)) 
	;;finished the task
	;; add current task to completed tasks
	(set! completed-tasks (cons current-task completed-tasks))
	;; remove task from assigned tasks
	(set! assigned-tasks (filter (lambda (ch) (not (char=? ch current-task))) 
				     assigned-tasks))
	;;(format #t "assigned-tasks = ~a ~%" assigned-tasks)
	(set! current-task-expires 0)
	(set! current-task #f)
	#t)
       (#t #f)))

    (define (worker-find-work )
      ;;(format #t "current-task ~a ~%" current-task)
      ;;(format #t " ~a " current-task)
      (cond
       ((not current-task) ;; find new work to do
	(let ((new-task (find-work task-list)))
	  (cond
	   ((char? new-task) 
	    (set! current-task new-task)
	    (set! current-task-expires (finish-time clock new-task))
	    new-task)
	   (#t #f))))))

    (lambda (op . args)
      (cond
       ((eq? op 'completed?) (work-completed?))
       ((eq? op 'get-task) current-task)
       ((eq? op 'get-task-list) task-list)
       ((eq? op 'find-work) (worker-find-work))
       (#t (error "worker message not understood" (list op args)))))))


(reset)

(define worker1 (make-worker task-list 1))
(define worker2 (make-worker task-list 2))
(define worker3 (make-worker task-list 3))
(define worker4 (make-worker task-list 4))
(define worker5 (make-worker task-list 5))

(define (sim)
  (reset)
  (do-while (< (length completed-tasks) 26)
	    (worker1 'completed?)
	    (worker2 'completed?)
	    (worker3 'completed?)
	    (worker4 'completed?)
	    (worker5 'completed?)
	    ;;(format #t "~a : ~a ~%" clock (reverse completed-tasks))
	    ;;(format #t "~a : " clock)
	    ;; complete all tasks 
	    (worker1 'find-work)
	    (worker2 'find-work)
	    (worker3 'find-work)
	    (worker4 'find-work)
	    (worker5 'find-work)
	    ;;(format #t " : ~a ~%" (reverse completed-tasks))
	    ;; find workers new work to do
	    (format #t "~a : ~a ~a ~a ~a ~a : ~a ~%" 
		    clock 
		    (worker1 'get-task)
		    (worker2 'get-task)
		    (worker3 'get-task)
		    (worker4 'get-task)
		    (worker5 'get-task)
		    (reverse completed-tasks))

	    (tick)))

;;(sim)

#|
(sim)

says
971

|#
	    


	 






























	   
