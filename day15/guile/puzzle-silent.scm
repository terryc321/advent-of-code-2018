;; -*- geiser-scheme-implementation: guile -*-

;;
;;
;; each elf goblin has 200 hits
;; each elf goblin also given an unique id number so we can talk about a specific entity
;; (find-elfs arr) =>  ((#<elf hits: 200 id: 6> 6 5) (#<elf hits: 200 id: 2> 5 3))
;;     elf with id 6 was last located at (6,5)
;;     elf with id 2 was last located at (5,3)
;; this way we can determine if we have moved and attacked with an entity and we do not move / attack with it
;; multiple times in a round
;;
;;


;; guile records (make-cave) does not work in the repl ?
;; works inside code imports modules
;; 

(use-modules (ice-9 optargs)) ;; optional args
(use-modules (system foreign)) ;; %null-pointer
(use-modules (system foreign-library))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 rdelim))
;; rdelim read-line 

(use-modules (ice-9 match))
(use-modules (ice-9 format))

(use-modules (ice-9 pretty-print)) 
(define pp pretty-print)

;; these modules are not in guile ecosystem they require an altered %load-path to make them visible
(use-modules (macros fcase))
(use-modules (macros inc))
(use-modules (macros array-loop)) ;; iterating over 2d array

(use-modules ((graphics sdl2 sdl) #:prefix sdl:))
(use-modules ((graphics sdl2 image) #:prefix img:))
(use-modules ((graphics cairo cairo) #:prefix cairo:))

(use-modules (srfi srfi-1))

;; current directory
;; (getcwd)
;; change directory
;; (chdir "day15/guile")

(use-modules (srfi srfi-9))
;; records srfi 

(use-modules (ice-9 optargs))
;;

(define (array-width arr) (second (first (array-shape arr))))
(define (array-height arr) (second (second (array-shape arr))))


;; assertions ?
(define assert
  (lambda (x)
    (if x #t (error "assertion failure <where? >"))))

;;(assert #f)

;; ;; no srfi-25 !!!! WHY !!!! instead guile has inbuilt procedures 
;; ;; should be a 2d array indexes starting from 1..2 , 1..2 
;; (define arr #2@1@1((1 2) (3 4)))
;; (array-ref arr 1 1) ;; expect 1
;; (array-ref arr 2 1) ;; expect 3
;; (array-ref arr 1 2) ;; expect 2
;; (array-ref arr 2 2) ;; expect 4

;; (define arr2 (make-array #\# '(1 3) '(1 3)))
;; ;; #2@1@1((#\# #\# #\#) (#\# #\# #\#) (#\# #\# #\#))
;; ;; meaning 2d array , indexes start at 1 in x direction , 1 in y direction
;; (array-set! arr2 'fred 1 1 )
;; ;; arr2

;; 
;; all correct 
;; so we can read a file a file , now lets translate this into a 2d array where we get get access to each element

;; 
(define (get-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let ((lines '()))
	(let loop ()
	  (let ((got (read-line (current-input-port))))
	    (cond
	     ((eof-object? got) #f)
	     (#t ;;(format #t "~a~%" got)
	      (set! lines (cons got lines))
	      (loop)))))
	(reverse lines)
	))))

;; removed any setters from goblin elf cave wall
;; simply need to make a new goblin or elf
;; this is because if an array holds a goblin / elf and stored in a list ie record the run
;; changing goblin internally will invalidate a recorded history of events ie run record will be garbage
;; mutation changes actual understanding of problem


(define make-wall (lambda ()
		    (lambda (op . args)
		      (cond
		       ((eq? op 'type) 'wall)
		       ((eq? op 'id) 0)))))
		       
(define wall? (lambda (x)
		(and (procedure? x)
		     (eq? 'wall (x 'type)))))

(define make-cave (lambda ()
		    (lambda (op . args)
		      (cond
		       ((eq? op 'type) 'cave)
		       ((eq? op 'id) 0)))))
		  
(define cave? (lambda (x)
		(and (procedure? x)
		     (eq? 'cave (x 'type)))))

;; make use of closures and way hide a unique id counter
(define make-elf  (lambda (hits id)
		    (lambda (op . args)
		      (cond
		       ((eq? op 'type) 'elf)
		       ((eq? op 'hits) hits)
		       ((eq? op 'show) (format #t "E<~a>~a" id hits))		       
		       ((eq? op 'as-string) (format #f "E<~a>~a" id hits))
		       ((eq? op 'id) id)))))

(define elf-hits (lambda (x)
		   (cond
		    ((elf? x) (x 'hits))
		    (#t (format #t "calling elf-hits - not an elf !~a~%" x)
			(error "elf-hits")))))

(define elf-id (lambda (x)
		   (cond
		    ((elf? x) (x 'id))
		    (#t (format #t "calling elf-id - not an elf !~a~%" x)
			(error "elf-id")))))



(define make-goblin (lambda (hits id)
		      (lambda (op . args)
			(cond
			 ((eq? op 'type) 'goblin)
			 ((eq? op 'hits) hits)
			 ((eq? op 'show) (format #t "G<~a>~a" id hits))		       
			 ((eq? op 'as-string) (format #f "G<~a>~a" id hits))
			 ((eq? op 'id) id)))))

(define goblin-hits (lambda (x)
		      (cond
		       ((goblin? x) (x 'hits))
		       (#t (format #t "calling goblin-hits - not an goblin !~a~%" x)
			   (error "goblin-hits")))))

(define goblin-id (lambda (x)
		   (cond
		    ((goblin? x) (x 'id))
		    (#t (format #t "calling goblin-id - not an goblin !~a~%" x)
			(error "goblin-id")))))


(define elf? (lambda (x)
		(and (procedure? x)
		     (eq? 'elf (x 'type)))))


(define goblin? (lambda (x)
		  (and (procedure? x)
		       (eq? 'goblin (x 'type)))))

(define generic-id (lambda (x)
		     (cond
		      ((procedure? x) (x 'id))		      
		      (#t (format #t "calling generic-id : object ~a is not a procedure~%" x)
			  (error "generic-id")))))



;; (let ((wall-secret 'kmlasdmfklasmdklfmsaldfmklsadf))
;;   (set! make-wall  (lambda ( a  . args)
;; 		     (cond
;; 		      ((eq? op 'create) 
;; 		      ((eq? op 'type) 'wall)
;; 		      ((eq? op 'wall?) (let ((lam (car args)))
;; 					 (eq? (lam) wall-secret)))



;; ;; template for learning define-record-type
;; (define-record-type employee-type
;;   (make-employee name age salary)
;;   employee?
;;   (name    get-employee-name)
;;   (age     get-employee-age    set-employee-age)
;;   (salary  get-employee-salary set-employee-salary))


;; elfs or goblins both get 200 hit points and 3 power 
;; (use-modules (ice-9 optargs))

;;      (define (make-window . args)
;;        (let-keywords args #f ((depth  screen-depth)
;;                               (bg     "white")
;;                               (width  800)
;;                               (height 100))
;;          ...))

;; Or, even more economically, like this:

;;      (use-modules (ice-9 optargs))

;;      (define* (make-window #:key (depth  screen-depth)
;;                                  (bg     "white")
;;                                  (width  800)
;;                                  (height 100))
;;        ...)


;; ;; seperated making different elements and distinguishing various elements
;; ;; we are free to change this representation at any time and nothing else needs to change
;; ;;(define make-wall (lambda () 'wall))
;; (define make-cave (lambda () 'cave))
;; (define make-elf (lambda () (list 'elf elf-hit-points elf-power )))
;; (define make-goblin (lambda () (list 'goblin goblin-hit-points goblin-power)))

;; (define wall? (lambda (x) (and (symbol? x) (eq? x 'wall))))
;; (define cave? (lambda (x) (and (symbol? x) (eq? x 'cave))))
;; (define elf? (lambda (x) (and (pair? x) (symbol? (car x)) (eq? (car x) 'elf))))
;; (define goblin? (lambda (x) (and (pair? x) (symbol? (car x)) (eq? (car x) 'goblin))))


(define elf-hit-points 200)
(define elf-power 3)
(define goblin-hit-points 200)
(define goblin-power 3)
;; hit points == health 


(define id (let ((counter 0))
	     (lambda ()
	       (set! counter (+ counter 1))
	       counter)))

(define (input filename)
  (let* ((str-lines (get-lines filename))
	 (nlines (length str-lines))
	 (nwidth (string-length (car str-lines)))
	 (all-nwidth (filter (lambda (n) (not (= n nwidth))) (map string-length str-lines))))
    ;; assertions survive compilation
    (assert (null? all-nwidth))
    ;; (format #t "all lines have width ~a~%" nwidth)
    ;; make an oversize array - using 1 indexing to N indexing !! 
    (let* ((w nwidth)
	   (h nlines)
	   (arr (make-array #\# (list 1 w) (list 1 h)))
	   (j 0))
      (map (lambda (str)
	     (let ((i 0))
	       (map (lambda (ch)
		      ;;(format #t "char at ~a ~a is ~a ~%" i j ch)
		      ;; must be elf E goblin G wall # or cavern . 
		      (assert (or (char=? ch #\#) (char=? ch #\.) (char=? ch #\E) (char=? ch #\G)))
		      (let ((cnv 
			     (cond 
			       ((char=? ch #\#) (make-wall))
			       ((char=? ch #\.) (make-cave))
			       ((char=? ch #\E) (make-elf elf-hit-points (id)))
			       ((char=? ch #\G) (make-goblin goblin-hit-points (id)))
			       (#t (error (format #f "bad char {~a} at {line ~a,col ~a}" ch j i))))))
			(array-set! arr cnv (+ i 1) (+ j 1)))
		      (set! i (+ i 1)))
		    (string->list (list-ref str-lines j))))
	     (set! j (+ j 1)))
	   str-lines)
      (values arr w h))))


;;(define arr (input "../example.txt"))
(define arr (input "../input.txt"))

;; wall?
;; cave?
;; elf?
;; goblin?



(define (output arr)
  (with-output-to-file "output.txt"
    (lambda ()
      (show-array arr))))

;; we can check we can output exactly same file as input.txt
;; (output (input))
;; diff ../input.txt output.txt
;; correctly rebuilds the maze

;; pads out to 4 spaces in total 
(define (pad x)
  (cond
   ((char? x) (format #f "   ~a" x))
   ((integer? x) (cond 
		  ((< x 10)  (format #f "   ~a" x))
		  ((< x 100) (format #f "  ~a" x))
		  (#t (format #f " ~a" x))))))
		  
;; abstracted out iterating over the array with ARRAY-LOOP macro 
(define* (show-array3 arr #:optional (port #t))
  (define xlim (array-width arr))
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((wall? elem) (format port "~a" (pad #\#)))
		   ((cave? elem) (format port "~a" (pad #\.)))
		   ((elf? elem) (format port "~a" (elem 'as-string)))
		   ((goblin? elem) (format port "~a" (elem 'as-string)))
		   ((integer? elem) (format port "~a" (pad elem)))
		   (#t (error "bad symbol")))
		  ;; end of line
		  (when (= x xlim)
		    (format port "~%")))))
  (array-loop arr foo))


(define* (show-array arr #:optional (port #t))
  (define xlim (array-width arr))
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((wall? elem) (format port "~a" (pad #\#)))
		   ((cave? elem) (format port "~a" (pad #\.)))
		   ((elf? elem) (format port "~a" (pad #\E)))
		   ((goblin? elem) (format port "~a" (pad #\G)))
		   ((integer? elem) (format port "~a" (pad elem)))
		   (#t (error "bad symbol")))
		  ;; end of line
		  (when (= x xlim)
		    (format port "~%")))))
  (array-loop arr foo))



(define* (show-array2 arr #:optional (port #t))
  (define xlim (array-width arr))
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((wall? elem) (format port "#"))
		   ((cave? elem) (format port "."))
		   ((elf? elem) (format port "E"))
		   ((goblin? elem) (format port "G"))
		   ((integer? elem) (format port "~a" elem))
		   (#t (error "bad symbol")))
		  ;; end of line
		  (when (= x xlim)
		    (format port "~%")))))
  (array-loop arr foo))


;; for the full input grid is 32 x 32 
;; (array-width arr)
;; (array-height arr)
;; (array-ref arr 1 1)
;; (array-ref arr 32 32)
;; (show-array arr)
;; (show-array2 arr)


(define (onboard? x y arr)
  (define xlim (array-width arr))
  (define ylim (array-height arr))
  (and (>= x 1) (<= x xlim)
       (>= y 1) (<= y ylim)))


(define (zero-array! arr)
  (define xlim (array-width arr))
  (define ylim (array-height arr))
  (define foo (lambda (arr x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((cave? elem) #f)
		   ((integer? elem) (array-set! arr (make-cave) x y))))))		 
  (array-loop arr foo)
  arr)


(define (array-copy arr)
  (define xlim (array-width arr))
  (define ylim (array-height arr))
  (define result (make-array #f (list 1 xlim) (list 1 ylim)))
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (array-set! result elem x y))))
  (array-loop arr foo)
  result)

(define (array-equal? arr arr2)
  (define xlim (array-width arr))
  (define ylim (array-height arr))
  (define xlim2 (array-width arr2))
  (define ylim2 (array-height arr2))
  (cond
   ((and (= xlim xlim2)(= ylim ylim2))
    (define exit #f)
    (define foo (lambda (ar x y)
		  (let ((elem (array-ref arr x y))
			(elem2 (array-ref arr2 x y)))
		    (when (not (equal? elem elem2))
		      (exit #f)))))
    (call-with-current-continuation
     (lambda (local-exit)
       ;; so when exit called above it will return #f for whole expression -shortcut 
       (set! exit local-exit)
       (array-loop arr foo)
       #t)))
   (#t ;; differ in dimensions
    #f)))


;; computing a breadth first search on the array does not change original array
;; this is important 
;; the original array should never be changed only copied and then copy should be altered
;; the computation can continue with a copy instead of changing the original and losing
;; coherence
(define (breadth-first-label-caves arr x y)
  " given array and coordinate of entity interested in where it can go
begins search on each square can reach vertically or horizontally
"
  (define arr2 (array-copy arr))
  (define (recur x y n)
    (when
	(onboard? x y arr)
      (let ((elem (array-ref arr2 x y)))
	(cond
	 ((wall? elem) #f)
	 ((cave? elem)
	  (array-set! arr2 n x y)
	  ;; recursively relabel each square based on distance 
	  (recur x (- y 1) (+ n 1))      
	  (recur (- x 1) y (+ n 1))
	  (recur (+ x 1) y (+ n 1))
	  (recur x (+ y 1) (+ n 1)))
	 ((elf? elem) #f)
	 ((goblin? elem) #f)
	 ((integer? elem) (cond
			   ((< elem n) #f) ;; square has better minima 
			   ((= elem n) #f) ;; also been here before
			   ((> elem n) ;; new low found
			    (array-set! arr2 n x y)	
			    (recur x (- y 1) (+ n 1))      
			    (recur (- x 1) y (+ n 1))
			    (recur (+ x 1) y (+ n 1))
			    (recur x (+ y 1) (+ n 1)))
			   ))))))
  ;;(format #t "arr2 = ~%")
  ;;(show-array arr2)
  ;; clean house
  (zero-array! arr2)
  
  ;;(format #t "zero'd = ~%")
  ;;(show-array arr2)
  ;; label each square 
  (recur (- x 1) y 1)
  (recur (+ x 1) y 1)
  (recur x (- y 1) 1)
  (recur x (+ y 1) 1)
  ;;; 
  ;;(format #t "result = ~%")
  ;; (show-array arr2)
  arr2
  )


(define (find-elfs-goblins arr)
  (define elfs '())
  (define goblins '())
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((elf? elem)    (set! elfs    (cons (list elem x y) elfs)))
		   ((goblin? elem) (set! goblins (cons (list elem x y) goblins)))))))
  (array-loop arr foo)
  (values elfs goblins))

(define (find-goblins arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      gobs)))

(define (find-elfs arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      efs)))

;; --- load entire file into repl -- compiling will not work --
;; --- bug in how records are implemented 
;;
;;
;; (show-array arr)
;; (define a (breadth-first-label-caves arr 1 1))
;; (show-array a)
;; (array-set! a (make-cave) 1 1)
;; (show-array a)
;; (show-array arr)
;; --- can confirm they are mutually independent (changing a does not change arr)
;; C-c C-b --- feed buffer to repl
;; ----


;; important thing for lexicographic list is the ID numbers id ,
;; this will determine what entity we are talking about
(define (lexicographic arr)
  "put them in order left to right , top to bottom"
  (define group '())
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((elf? elem)    (set! group   (cons (list elem x y) group )))
		   ((goblin? elem) (set! group (cons (list elem x y) group)))))))
  (array-loop arr foo)
  (reverse group))




(define (get-elf-neighbours arr x y)
  (let ((result (list (let ((dx (- x 1))(dy y))
			(if (and (onboard? dx dy arr) (elf? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'left)  'none))
		      (let ((dx (+ x 1))(dy y))
			(if (and (onboard? dx dy arr) (elf? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'right)  'none))
		      (let ((dx x) (dy (- y 1)))
			(if (and (onboard? dx dy arr) (elf? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'up)  'none))
		      (let ((dx x) (dy (+ y 1)))
			(if (and (onboard? dx dy arr) (elf? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'down)  'none)))))
    (filter (lambda (x) (not (eq? x 'none))) result)))

		      

;; the 3 phase attack plan
;; phase-one   : attack if possible otherwise move to phase two
;; phase-two   : move nearer to an elf if possible
;; phase-three : attack if possible
		      


;; phase one must return a usable array
(define (move-goblin-phase-one arr entity x y)
  ;; (format #t "phase one goblin {~a}~%" (entity 'as-string))
  ;; (show-array arr)
  ;; (format #t "phase one goblin : looking for elfs around ~a ~a~%" x y)    
  ;; get all the elfs around x y
  (let ((elfs (get-elf-neighbours arr x y)))
    ;; (format #t "phase-one goblin : there are ~a elfs in the neighbourhood~%" (length elfs))
    
    ;; sort by weakest elf
    (set! elfs (sort elfs (lambda (x y) (< (elf-hits (car x)) (elf-hits (car y))))))
    ;; (format #t "phase-one goblin : there are ~a sorted elves ~%" (length elfs))
    ;; (format #t "phase-one goblin : their strengths are ~a~%" (map (lambda (elfxy) (elf-hits (car elfxy))) elfs))
    
    
    ;; filter out any elfs stronger than weakest
    (set! elfs (filter (lambda (x) (<= (elf-hits (car x)) (elf-hits (car (car elfs)))))   elfs))
    ;; (format #t "phase-one goblin : there are ~a weak elves ~%" (length elfs))

    ;; pick the elf lexicographic ordering
    (cond
     ((null? elfs)
      ;; (format #t "phase one goblin : no elfs found in vicinity of ~a ~a ~%" x y)
      (move-goblin-phase-two arr entity x y))
     (#t (let ((ups    (filter (lambda (x) (eq? 'up (fourth x))) elfs))
	       (downs  (filter (lambda (x) (eq? 'down (fourth x))) elfs))
	       (lefts  (filter (lambda (x) (eq? 'left (fourth x))) elfs))
	       (rights (filter (lambda (x) (eq? 'right (fourth x))) elfs)))
	   ;; (format #t "phase one goblin : ups ~a : downs ~a : lefts ~a : rights ~a ~%"  ups downs lefts rights)
	   (cond
	    ((not (null? ups))
	     ;; (format #t "debug : ups . ~%")
	     (let* ((entity-xydir2 (car ups))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : ups II . ~%")	     
	       (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    
	    ((not (null? lefts))
	     ;; (format #t "debug : lefts . ~%")
	     (let* ((entity-xydir2 (car lefts))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : lefts II . ~%")	     
	       (goblin-attack-elf arr entity x y entity2 x2 y2)))

	    ((not (null? rights))
	     ;; (format #t "debug : rights . ~%")
	     (let* ((entity-xydir2 (car rights))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : rights II . ~%")	       
	       (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    
	    ((not (null? downs))
	     ;; (format #t "debug : downs . ~%")
	     (let* ((entity-xydir2 (car downs))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : downs II . ~%")	     	       
	       (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    (#t (format #t "inside move-goblin-phase-one Line 536 how did we get here")
		(error "move-goblin-phase-one lost at sea"))))))))


;; should we check that entity2 is an elf at x2 y2 ?
(define (goblin-attack-elf arr entity x y entity2 x2 y2)
  ;; (format #t "< goblin-attack-elf > ~%")
  (let ((arr2 (array-copy arr)))
    (let* ((hits (elf-hits entity2))
	   (id (elf-id entity2))
	   (after (- hits 3)))
      ;; (format #t "< goblin-attack-elf : hits ~a : id ~a : after ~a > ~%" hits id after)
      (cond
       ((<= after 0) (array-set! arr2 (make-cave) x2 y2))
       (#t (array-set! arr2 (make-elf after id) x2 y2)))
      arr2)))


;; what is phase two ?? -- goblin just finds a direction to move --> 
;; phase two must return a usable array 
(define (move-goblin-phase-two arr entity x y)
  ;; (format #t "phase two goblin {~a}~%" (entity 'as-string))
  ;; (show-array arr)
  ;; (format #t "phase two goblin : proceeding with flood~%")    
  (let ((elfsxy (find-elfs arr)))
    ;; (format #t "got elfsxy as ~%")
    ;; (pp elfsxy)
    ;; (format #t "proceeding to flood fill demonstration~%")
    ;; for each elf location do a flood fill and determine what is best direction to move
    (let ((left #f)(right #f)(up #f)(down #f))
      (map (lambda (elemxy)	     
	     (let ((elem (first elemxy))
		   (x2 (second elemxy))
		   (y2 (third elemxy)))
	       (let ((arr2 (breadth-first-label-caves arr x2 y2)))
		 ;; (format #t "numbered for elf at ~a ~a ~%" x2 y2)
		 ;; (show-array arr2)
		 ;; (format #t "~%")
		 ;; LEFT
		 (when (onboard? (- x 1) y arr)
		   (let ((estar (array-ref arr2 (- x 1) y)))
		     (when (integer? estar)
		       (cond
			((not left) (set! left estar))
			((< estar left) (set! left estar))))))
		 ;;RIGHT
		 (when (onboard? (+ x 1) y arr)
		   (let ((estar (array-ref arr2 (+ x 1) y)))
		     (when (integer? estar)
		       (cond
			((not right) (set! right estar))
			((< estar right) (set! right estar))))))
		 ;; UP
		 (when (onboard? x (- y 1) arr)
		   (let ((estar (array-ref arr2 x (- y 1))))
		     (when (integer? estar)
		       (cond
			((not up) (set! up estar))
			((< estar up) (set! up estar))))))
		 ;; DOWN
		 (when (onboard? x (+ y 1) arr)
		   (let ((estar (array-ref arr2 x (+ y 1))))
		     (when (integer? estar)
		       (cond
			((not down) (set! down estar))
			((< estar down) (set! down estar))))))
		 ;; present findings 
		 ;; (format #t "flood data : left ~a : right ~a : up ~a : down ~a ~%~%"left right up down)
		 )))
	   elfsxy)
      (let ((dirs `((left ,left)(right ,right)(down ,down)(up ,up))))
	;; remove any #f symbols from dirs
	(set! dirs (filter (lambda (x) (not (eq? #f (second x)))) dirs))
	;; sort based on how close elf is to goblin
	(set! dirs (sort dirs (lambda (x y) (< (second x)(second y)))))
	;; sort based on same as best found in terms of distance
	(set! dirs (filter (lambda (x) (= (second x) (second (first dirs)))) dirs))
	(let ((lefts (filter (lambda (x) (eq? (first x) 'left)) dirs))
	      (rights (filter (lambda (x) (eq? (first x) 'right)) dirs))
	      (ups (filter (lambda (x) (eq? (first x) 'up)) dirs))
	      (downs (filter (lambda (x) (eq? (first x) 'down)) dirs)))
	  ;; CAREFUL only apply array-set! to ARR2 not original ARR  !!!
	  (cond ;; did not move and no enemy in reach 
	   ((null? dirs)
	    ;; (format #t "goblin did not MOVE ~%")
	    (move-goblin-phase-three arr entity x y))
	   ((not (null? ups)) ;; go up -
	    ;; (format #t "goblin moved UP ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity x (- y 1))
	      (array-set! arr2 (make-cave) x y)
	      (move-goblin-phase-three arr2 entity x (- y 1))))
	   ((not (null? lefts)) ;; go left
	    ;; (format #t "goblin moved LEFT ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity (- x 1) y)
	      (array-set! arr2 (make-cave) x y)
	      (move-goblin-phase-three arr2 entity (- x 1) y)))
	   ((not (null? rights)) ;; go right
	    ;; (format #t "goblin moved RIGHT ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity (+ x 1) y)
	      (array-set! arr2 (make-cave) x y)
	      (move-goblin-phase-three arr2 entity (+ x 1) y)))
	   ((not (null? downs)) ;; go down
	    ;; (format #t "goblin moved DOWN ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity x (+ y 1))
	      (array-set! arr2 (make-cave) x y)
	      (move-goblin-phase-three arr2 entity x (+ y 1))))
	   (#t (error "HUH???"))))))))


;; phase three must return a usable array
(define (move-goblin-phase-three arr entity x y)
  ;; (format #t "phase three goblin {~a}~%" (entity 'as-string))
  ;; (show-array arr)
  ;; (format #t "phase three goblin : looking for elfs around ~a ~a~%" x y)  
  ;; get all the elfs around x y
  (let ((elfs (get-elf-neighbours arr x y)))
    ;; sort by weakest elf
    (set! elfs (sort elfs (lambda (x y) (< (elf-hits (car x))
					   (elf-hits (car y))))))
    ;; filter out any elfs stronger than weakest
    (set! elfs (filter (lambda (x) (> (elf-hits (car x))
				      (elf-hits (car (car elfs)))))
		       elfs))
    ;; pick the elf lexicographic ordering
    ;; elfs really elfs-xydir 
    (cond
     ((null? elfs)
      ;; (format #t "phase three goblin : no elfs in sight !~%")
      arr)     ;; we have moved goblin - no elf in range
     (#t (let ((ups (filter (lambda (x) (eq? 'up (fourth x))) elfs))
	       (downs (filter (lambda (x) (eq? 'down (fourth x))) elfs))
	       (lefts (filter (lambda (x) (eq? 'left (fourth x))) elfs))
	       (rights (filter (lambda (x) (eq? 'right (fourth x))) elfs)))
	   (cond
	    (ups (let* ((entity-xydir2 (car ups))
			(entity2 (car entity-xydir2))
			(x2 (second entity-xydir2))
			(y2 (third entity-xydir2)))
		   ;; (format #t "phase three goblin : attack on elf UP~%")
		   (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    (lefts (let* ((entity-xydir2 (car lefts))
			(entity2 (car entity-xydir2))
			(x2 (second entity-xydir2))
			(y2 (third entity-xydir2)))
		     ;; (format #t "phase three goblin : attack on elf LEFT~%")		     
		     (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    (rights (let* ((entity-xydir2 (car rights))
			   (entity2 (car entity-xydir2))
			   (x2 (second entity-xydir2))
			   (y2 (third entity-xydir2)))
		      ;; (format #t "phase three goblin : attack on elf RIGHT~%")		      
		      (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    (downs (let* ((entity-xydir2 (car downs))
			  (entity2 (car entity-xydir2))
			  (x2 (second entity-xydir2))
			  (y2 (third entity-xydir2)))
		     ;; (format #t "phase three goblin : attack on elf DOWN~%")		     
		     (goblin-attack-elf arr entity x y entity2 x2 y2)))
	    (#t (format #t "inside move-goblin-phase-three Line 722 how did we get here")
		(error "move-goblin-phase-three lost at sea"))))))))


;; the 3 phase attack plan
;; phase-one   : attack if possible otherwise move to phase two
;; phase-two   : move nearer to an elf if possible
;; phase-three : attack if possible
		      
;; should we check that entity2 is an elf at x2 y2 ?
(define (elf-attack-goblin arr entity x y entity2 x2 y2)
  ;; (format #t "< elf-attack-goblin > ~%")
  (let ((arr2 (array-copy arr)))
    (let* ((hits (goblin-hits entity2))
	   (id (goblin-id entity2))
	   (after (- hits 3)))
      ;; (format #t "< elf-attack-goblin : hits ~a : id ~a : after ~a > ~%" hits id after)
      (cond
       ((<= after 0) (array-set! arr2 (make-cave) x2 y2))
       (#t (array-set! arr2 (make-goblin after id) x2 y2)))
      arr2)))


(define (get-goblin-neighbours arr x y)
  (let ((result (list (let ((dx (- x 1))(dy y))
			(if (and (onboard? dx dy arr) (goblin? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'left)  'none))
		      (let ((dx (+ x 1))(dy y))
			(if (and (onboard? dx dy arr) (goblin? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'right)  'none))
		      (let ((dx x) (dy (- y 1)))
			(if (and (onboard? dx dy arr) (goblin? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'up)  'none))
		      (let ((dx x) (dy (+ y 1)))
			(if (and (onboard? dx dy arr) (goblin? (array-ref arr dx dy)))
			    (list (array-ref arr dx dy) dx dy 'down)  'none)))))
    (filter (lambda (x) (not (eq? x 'none))) result)))


;; phase one must return a usable array
(define (move-elf-phase-one arr entity x y)
  ;; (format #t "phase one elf {~a}~%" (entity 'as-string))
  ;; (show-array arr)
  ;; (format #t "phase one elf : looking for goblins around ~a ~a~%" x y)    
  ;; get all the goblins around x y
  (let ((gobs (get-goblin-neighbours arr x y)))
    ;; (format #t "phase-one elf : there are ~a goblins in the neighbourhood~%" (length gobs))
    
    ;; sort by weakest elf
    (set! gobs (sort gobs (lambda (x y) (< (goblin-hits (car x)) (goblin-hits (car y))))))
    ;; (format #t "phase-one elf : there are ~a sorted goblins ~%" (length gobs))
    ;; (format #t "phase-one elf : their strengths are ~a~%" (map (lambda (gobxy) (goblin-hits (car gobxy))) gobs))
        
    ;; filter out any gobs stronger than weakest
    (set! gobs (filter (lambda (x) (<= (goblin-hits (car x)) (goblin-hits (car (car gobs)))))   gobs))
    ;; (format #t "phase-one elf : there are ~a weak goblins ~%" (length gobs))

    ;; pick the elf lexicographic ordering
    (cond
     ((null? gobs)
      ;; (format #t "phase one elf : no goblins found in vicinity of ~a ~a ~%" x y)
      (move-elf-phase-two arr entity x y))
     (#t (let ((ups    (filter (lambda (x) (eq? 'up (fourth x))) gobs))
	       (downs  (filter (lambda (x) (eq? 'down (fourth x))) gobs))
	       (lefts  (filter (lambda (x) (eq? 'left (fourth x))) gobs))
	       (rights (filter (lambda (x) (eq? 'right (fourth x))) gobs)))
	   ;; (format #t "phase one elf : ups ~a : downs ~a : lefts ~a : rights ~a ~%"  ups downs lefts rights)
	   ;; (format #t "phase one elf : gobs => ~a ~% " gobs)
	   (cond
	    ((not (null? ups))
	     ;; (format #t "debug : ups . ~%")
	     (let* ((entity-xydir2 (car ups))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : ups II . ~%")	     
	       (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    
	    ((not (null? lefts))
	     ;; (format #t "debug : lefts . ~%")
	     (let* ((entity-xydir2 (car lefts))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : lefts II . ~%")	     
	       (elf-attack-goblin arr entity x y entity2 x2 y2)))

	    ((not (null? rights))
	     ;; (format #t "debug : rights . ~%")
	     (let* ((entity-xydir2 (car rights))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : rights II . ~%")	       
	       (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    
	    ((not (null? downs))
	     ;; (format #t "debug : downs . ~%")
	     (let* ((entity-xydir2 (car downs))
		    (entity2 (car entity-xydir2))
		    (x2 (second entity-xydir2))
		    (y2 (third entity-xydir2)))
	       ;; (format #t "debug : downs II . ~%")	     	       
	       (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    (#t (format #t "inside move-elf-phase-one Line 536 how did we get here")
		(error "move-elf-phase-one lost at sea"))))))))



;; phase two must return a usable array 
(define (move-elf-phase-two arr entity x y)
  ;; (format #t "phase two elf {~a}~%" (entity 'as-string))
  ;; (show-array arr)
  ;; (format #t "phase two elf : proceeding with flood~%")    
  (let ((gobsxy (find-goblins arr)))
    ;; (format #t "got gobsxy as ~%")
    ;; (pp gobsxy)
    ;; (format #t "proceeding to flood fill demonstration~%")
    ;; for each goblin location do a flood fill and determine what is best direction to move
    (let ((left #f)(right #f)(up #f)(down #f))
      (map (lambda (elemxy)	     
	     (let ((elem (first elemxy))
		   (x2 (second elemxy))
		   (y2 (third elemxy)))
	       (let ((arr2 (breadth-first-label-caves arr x2 y2)))
		 ;; (format #t "numbered for elf at ~a ~a ~%" x2 y2)
		 ;; (show-array arr2)
		 ;; (format #t "~%")
		 ;; LEFT
		 (when (onboard? (- x 1) y arr)
		   (let ((estar (array-ref arr2 (- x 1) y)))
		     (when (integer? estar)
		       (cond
			((not left) (set! left estar))
			((< estar left) (set! left estar))))))
		 ;;RIGHT
		 (when (onboard? (+ x 1) y arr)
		   (let ((estar (array-ref arr2 (+ x 1) y)))
		     (when (integer? estar)
		       (cond
			((not right) (set! right estar))
			((< estar right) (set! right estar))))))
		 ;; UP
		 (when (onboard? x (- y 1) arr)
		   (let ((estar (array-ref arr2 x (- y 1))))
		     (when (integer? estar)
		       (cond
			((not up) (set! up estar))
			((< estar up) (set! up estar))))))
		 ;; DOWN
		 (when (onboard? x (+ y 1) arr)
		   (let ((estar (array-ref arr2 x (+ y 1))))
		     (when (integer? estar)
		       (cond
			((not down) (set! down estar))
			((< estar down) (set! down estar))))))
		 ;; present findings 
		 ;; (format #t "flood data : left ~a : right ~a : up ~a : down ~a ~%~%"left right up down)
		 )))
	   gobsxy)
      (let ((dirs `((left ,left)(right ,right)(down ,down)(up ,up))))
	;; remove any #f symbols from dirs
	(set! dirs (filter (lambda (x) (not (eq? #f (second x)))) dirs))
	;; sort based on how close elf is to elf
	(set! dirs (sort dirs (lambda (x y) (< (second x)(second y)))))
	;; sort based on same as best found in terms of distance
	(set! dirs (filter (lambda (x) (= (second x) (second (first dirs)))) dirs))
	(let ((lefts (filter (lambda (x) (eq? (first x) 'left)) dirs))
	      (rights (filter (lambda (x) (eq? (first x) 'right)) dirs))
	      (ups (filter (lambda (x) (eq? (first x) 'up)) dirs))
	      (downs (filter (lambda (x) (eq? (first x) 'down)) dirs)))
	  ;; CAREFUL only apply array-set! to ARR2 not original ARR  !!!
	  (cond ;; did not move and no enemy in reach 
	   ((null? dirs)
	    ;; (format #t "elf did not MOVE ~%")
	    (move-elf-phase-three arr entity x y))
	   ((not (null? ups)) ;; go up -
	    ;; (format #t "elf moved UP ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity x (- y 1))
	      (array-set! arr2 (make-cave) x y)
	      (move-elf-phase-three arr2 entity x (- y 1))))
	   ((not (null? lefts)) ;; go left
	    ;; (format #t "elf moved LEFT ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity (- x 1) y)
	      (array-set! arr2 (make-cave) x y)
	      (move-elf-phase-three arr2 entity (- x 1) y)))
	   ((not (null? rights)) ;; go right
	    ;; (format #t "elf moved RIGHT ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity (+ x 1) y)
	      (array-set! arr2 (make-cave) x y)
	      (move-elf-phase-three arr2 entity (+ x 1) y)))
	   ((not (null? downs)) ;; go down
	    ;; (format #t "elf moved DOWN ~%")
	    (let ((arr2 (array-copy arr)))
	      (array-set! arr2 entity x (+ y 1))
	      (array-set! arr2 (make-cave) x y)
	      (move-elf-phase-three arr2 entity x (+ y 1))))
	   (#t (error "HUH???"))))))))



;; phase three must return a usable array
(define (move-elf-phase-three arr entity x y)
  ;; (format #t "phase three elf {~a}~%" (entity 'as-string))
  ;; (show-array arr)
  ;; (format #t "phase three elf : looking for elfs around ~a ~a~%" x y)  
  ;; get all the elfs around x y
  (let ((elfs (get-elf-neighbours arr x y)))
    ;; sort by weakest elf
    (set! elfs (sort elfs (lambda (x y) (< (elf-hits (car x))
					   (elf-hits (car y))))))
    ;; filter out any elfs stronger than weakest
    (set! elfs (filter (lambda (x) (> (elf-hits (car x))
				      (elf-hits (car (car elfs)))))
		       elfs))
    ;; pick the elf lexicographic ordering
    ;; elfs really elfs-xydir 
    (cond
     ((null? elfs)
      ;; (format #t "phase three elf : no elfs in sight !~%")
      arr)     ;; we have moved elf - no elf in range
     (#t (let ((ups (filter (lambda (x) (eq? 'up (fourth x))) elfs))
	       (downs (filter (lambda (x) (eq? 'down (fourth x))) elfs))
	       (lefts (filter (lambda (x) (eq? 'left (fourth x))) elfs))
	       (rights (filter (lambda (x) (eq? 'right (fourth x))) elfs)))
	   (cond
	    (ups (let* ((entity-xydir2 (car ups))
			(entity2 (car entity-xydir2))
			(x2 (second entity-xydir2))
			(y2 (third entity-xydir2)))
		   ;; (format #t "phase three elf : attack on elf UP~%")
		   (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    (lefts (let* ((entity-xydir2 (car lefts))
			(entity2 (car entity-xydir2))
			(x2 (second entity-xydir2))
			(y2 (third entity-xydir2)))
		     ;; (format #t "phase three elf : attack on elf LEFT~%")		     
		     (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    (rights (let* ((entity-xydir2 (car rights))
			   (entity2 (car entity-xydir2))
			   (x2 (second entity-xydir2))
			   (y2 (third entity-xydir2)))
		      ;; (format #t "phase three elf : attack on elf RIGHT~%")		      
		      (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    (downs (let* ((entity-xydir2 (car downs))
			  (entity2 (car entity-xydir2))
			  (x2 (second entity-xydir2))
			  (y2 (third entity-xydir2)))
		     ;; (format #t "phase three elf : attack on elf DOWN~%")		     
		     (elf-attack-goblin arr entity x y entity2 x2 y2)))
	    (#t (format #t "inside move-elf-phase-three Line 722 how did we get here")
		(error "move-elf-phase-three lost at sea"))))))))




;; if doing it in order , an entity may be killed , but then should generate a new array arr
;; but lexicographic will be looking for entity 
;; so we will have to return a new array based on each entities move 
;; or if no movement we can return original
;; we have to be careful if we mutate the array 

(define (task a)  
  (let loop ((order (lexicographic a)) (arr (array-copy a)))
    (cond
     ((null? order) arr)
     (#t
      (let* ((entity-xy (car order))
	     (entity (car entity-xy))
	     (x (car (cdr entity-xy)))
	     (y (car (cdr (cdr entity-xy)))))
	;; (format #t "TASK LOOP : PROCESSING ~a AT ~a ~a~%" (entity 'as-string) x y)
	(let ((entity2 (array-ref arr x y)))
	  (cond
	   ((= (generic-id entity) (generic-id entity2))
	    (cond
	     ((elf? entity)
	      ;; (format #t "its an ELF !~%")
	      (let ((new-arr (move-elf-phase-one arr entity x y)))
		(loop (cdr order) new-arr)))
	     ((goblin? entity)
	      ;; (format #t "its an GOBLIN !~%")
	      (let ((new-arr (move-goblin-phase-one arr entity x y)))
		(loop (cdr order) new-arr)))
	     ((cave? entity)
	      (loop (cdr order) arr))
	     (#t
	      (format #t "UNCERTAIN ... UNCERTAIN .. TASK .... UNCERTAIN ~%")
		 (error "UNCERTAIN")
		 (loop (cdr order) arr))))
	   (#t
	    ;; (format #t "TASK : processing : GENERIC-ID mis match with CACHED entity~%")
	    ;; (format #t "cached identity ~a " (generic-id entity))
	    ;; (format #t " : array ref identity ~a " (generic-id entity2))
	    ;; probably a cave - respond identity 0
	    ;; possibly another entity ??
	    ;; the only reason may be entity died
	    (loop (cdr order) arr)))))))))


;; debugging experience overall 
;; interesting experiment to dump it out as an org mode document where can see 
;; then it can get rendered to webpage for future examination


;; task == a round
;; keep applying task until we run out of changes ??
(define (run arr lim)
  (let ((xs (list (list 0 arr))))    
    (letrec ((loop (lambda (arr n)
		     (let ((out (task arr)))
		       (format #t "~% *********** AFTER <ROUND ~a> ************** ~%" n)
		       (show-array2 out)
		       (set! xs (cons (list n out) xs))
		       (cond
			((< n lim) (loop out (+ n 1)))
			(#t (reverse xs)))))))
      (loop arr 1))))



(define (go)
  (let ((xs (run arr 50)))
    (format #t "displaying results of the run ~%")
    (format #t "the length of xs is ~a ~%" (length xs))
    (map (lambda (x)
	   (format #t "expecting x is a pair ? ~a~%" (pair? x))
	   (format #t "expecting cdr.x is a pair ? ~a~%" (pair? (cdr x)))
	   (format #t "length of x is ~a ~%" (length x))
	   (let ((n (car x))
		 (arr (car (cdr x))))
	     (format #t "n = ~a ~%" n)
	     ;;(format #t "arr = ~a ~%" myarr)
	     ;; (format #t "array? = ~a ~%" (array? myarr))	     
	     (format #t "~%**********************************~%")
	     (format #t "AFTER ROUND ~a ~%" n)
	     (show-array arr)
	     (format #t "~%")))
	 xs)
    #t))









		     





