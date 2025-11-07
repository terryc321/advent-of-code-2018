;; -*- geiser-scheme-implementation: guile -*-

#|

arr
(task arr) => determines using breadth first search which squares to find
TODO now we need to look at say goblin we are working with , find all elfs with lowest number around them ,
 we need to retrace paths back and find if we can reach our original elf ??

TODO -- sort attack of opponents within 1 square , by health , by vertical Y , by horizontal === lexicographic

then should get a result <>



enjoy experience trying to load my own sdl ffi library into guile scheme - from game1 

load + compile game1.scm first , since everything is practically at toplevel and open 

|#


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



(define (input)
  (let* ((str-lines (get-lines "../input.txt"))
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
			       ((char=? ch #\E) (make-elf))
			       ((char=? ch #\G) (make-goblin))
			       (#t (error (format #f "bad char {~a} at {line ~a,col ~a}" ch j i))))))
			(array-set! arr cnv (+ i 1) (+ j 1)))
		      (set! i (+ i 1)))
		    (string->list (list-ref str-lines j))))
	     (set! j (+ j 1)))
	   str-lines)
      (values arr w h))))

;; seperated making different elements and distinguishing various elements
;; we are free to change this representation at any time and nothing else needs to change
(define make-wall (lambda () 'wall))
(define make-cave (lambda () 'cave))
(define make-elf (lambda () (list 'elf 0)))
(define make-goblin (lambda () (list 'goblin 0)))

(define wall? (lambda (x) (and (symbol? x) (eq? x 'wall))))
(define cave? (lambda (x) (and (symbol? x) (eq? x 'cave))))
(define elf? (lambda (x) (and (pair? x) (symbol? (car x)) (eq? (car x) 'elf))))
(define goblin? (lambda (x) (and (pair? x) (symbol? (car x)) (eq? (car x) 'goblin))))


(define arr (input))

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
(define (show-array arr)
  (define xlim (array-width arr))
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((wall? elem) (format #t "~a" (pad #\#)))
		   ((cave? elem) (format #t "~a" (pad #\.)))
		   ((elf? elem) (format #t "~a" (pad #\E)))
		   ((goblin? elem) (format #t "~a" (pad #\G)))
		   ((integer? elem) (format #t "~a" (pad elem)))
		   (#t (error "bad symbol")))
		  ;; end of line
		  (when (= x xlim)
		    (format #t "~%")))))
  (array-loop arr foo))
	      	    

;; (define show-array 
;;   (lambda (arr)  ;; how get width height of generated array in guile ?
;;     (let ((width 32)(height 32))
;;       (let loopy ((y 1))
;; 	(when  (<= y height)
;; 	  (let loopx ((x 1))
;; 	    (when  (<= x width)
;; 	      (let ((elem (array-ref arr x y)))
;; 		(cond
;; 		 ((wall? elem) (format #t "~a" #\#))
;; 		 ((cave? elem) (format #t "~a" #\.))
;; 		 ((elf? elem) (format #t "~a" #\E))
;; 		 ((goblin? elem) (format #t "~a" #\G))
;; 		 (#t (error "bad symbol"))))
;; 	      ;; next x 
;; 	      (loopx (+ x 1)))) ;; let loopx
;; 	  ;; newline 
;; 	  (when (<= y height)
;; 	    (format #t "~%"))
;; 	  ;; next y	    
;; 	  (loopy (+ y 1)))) ;; let loopy
;;       )))




;;  ;; for a given array - find all goblins - find all elfs
;; (define find-elfs-goblins
;;   (lambda (arr)  ;; how get width height of generated array in guile ?    
;;     (let ((width 32)(height 32)
;; 	  (elfs '())
;; 	  (goblins '()))
;;       (let loopy ((y 1))
;; 	(when  (<= y height)
;; 	  (let loopx ((x 1))
;; 	    (when  (<= x width)
;; 	      (let ((symbol (array-ref arr x y)))
;; 		(cond
;; 		 ((eq? symbol 'wall) #f)
;; 		 ((eq? symbol 'cave) #f)
;; 		 ((eq? symbol 'elf)  (set! elfs (cons (list x y) elfs))  )
;; 		 ((eq? symbol 'goblin) (set! goblins (cons (list x y) goblins)) )
;; 		 (#t (error "bad symbol"))))
;; 	      ;; next x 
;; 	      (loopx (+ x 1)))) ;; let loopx
;; 	  ;; newline 
;; 	  ;; (when (<= y height)
;; 	  ;;   ;; (format #t "~%")
;; 	  ;;   #t
;; 	  ;;   )
;; 	  ;; next y	    
;; 	  (loopy (+ y 1)))) ;; let loopy
;;       (values elfs goblins))))


(define (find-elfs-goblins arr)
  (define elfs '())
  (define goblins '())
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((elf? elem)    (set! elfs    (cons (append elem (list x y)) elfs)))
		   ((goblin? elem) (set! goblins (cons (append elem (list x y)) goblins)))))))
  (array-loop arr foo)
  (values elfs goblins))



(define (goblins arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      (map (lambda (p)
	     (when (elf? p)
	       (match p
		 ((goblin n x y)		   
		  (format #t "GOBLIN at {~a,~a} is {~a}~%" x y (array-ref arr x y))))))
	   gobs)
      gobs)))


(define (elfs arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      (map (lambda (p)
	     (when (elf? p)
	       (match p
		 ((elf n x y)		   
		  (format #t "ELF at {~a,~a} is {~a}~%" x y (array-ref arr x y))))))
	   gobs)
      gobs)))



#|

determine shortest path between an elf and a goblin
determine shortest paths between an? goblin and elfs
any elf in reach of a goblin < > ^ v not diagonally , then attacks rather than moves
does the shortest path move around an already present E elf or G goblin ?

need to make elfs and goblins records or objects as they also now have properties
such as health 

sort peeps into lexicographic order , left to right , top to bottom 
pick first to move .
find all enemies reachable
- some aspect of symmetry here ? but order lexicographic so may not be symmetric
rinse repeat with others...

find all paths using breadth first approach 
only move through empty cave? squares not through players

|#


(define (lexicographic arr)
  "put them in order left to right , top to bottom"
  (define group '())
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (cond
		   ((elf? elem)    (set! group   (cons (append elem (list x y)) group )))
		   ((goblin? elem) (set! group (cons (append elem (list x y)) group)))))))
  (array-loop arr foo)
  (reverse group))

;; copy arr
;;(define (array-copy arr) #t)
(define (array-copy arr)
  (define xlim (array-width arr))
  (define ylim (array-height arr))
  (define result (make-array (make-wall) (list 1 xlim) (list 1 ylim)))
  (define foo (lambda (ar x y)
		(let ((elem (array-ref arr x y)))
		  (array-set! result elem x y))))
  (array-loop arr foo)
  result)


;; check equal?
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

  


;; still not convinced how array-copy works , shown some evidence of aliasing 
;; (define t1 (make-array 0 '(1 2) '(1 2)))
;; t1
;; (array-set! t1 1 1 1)
;; (array-set! t1 2 2 1)
;; (array-set! t1 3 1 2)
;; (array-set! t1 4 2 2)
;; t1

;; ;;#2@1@1( (1 2)(3 4)))
;; (define t2 (array-copy t1))
;; (format #t "~a~%" t1)
;; (format #t "~a~%" t2)
;; (equal? t1 t2)
;; (array-equal? t1 t2)
;; (array-set! t1 5 1 1) ;; 5 to 1,1
;; (array-ref t1 1 1)
;; (array-ref t2 1 1)
;; (array-equal? t1 t2)

;; ;; array-copy does not actually do a correct copy ! aliasing of memory space
;; (define arr2 (array-copy arr))
;; (define arr3 (array-copy arr))
;; (array-equal? arr2 arr3)
;; (array-set! arr2 'differ 32 32)
;; (array-ref arr2 32 32)
;; (array-ref arr3 32 32)
;; (array-equal? arr2 arr3)


;; how do we record where it should go - do we return it as a list of elements
;; do we copy array again .. we can reuse it
;; let #f be an empty square
;; when used - place an integer there
;; 
(define (breadth-first elem)
  (match elem
    ((type n x y) (format #t "breadth on {~a} with health ~a at {~a , ~a} ~%" type n x y))))

(define (array-width arr) (second (first (array-shape arr))))
(define (array-height arr) (second (second (array-shape arr))))


;; (map breadth-first (lexicographic arr))
;; h health : x pos : y pos : type = elf / goblin : 
;; result will hold positions of next move each goblin or elf
(define (task arr)
  (let* ((arr2 (array-copy arr)) ;; reuse arr2 over and over as a scratch area 
	 (xlim (array-width arr2))
	 (ylim (array-height arr2))
	 (result '()))
    (define (breadth-first elem)
      (format #t "breadth first on ~a ~%" elem)
      (zero-array! arr2)
      (match elem
	((type h x y) (recur-entry type h x y))))
    (define (onboard? x y)
      (and (>= x 1)(<= x xlim)
	   (>= y 1)(<= y ylim)))
    (define (opponent? type x y)
      (and (onboard? x y)
	   (or
	    (and (elf? type) (goblin? (array-ref arr x y)))
	    (and (goblin? type) (elf? (array-ref arr x y))))))
    (define (recur-entry type h x y)
      (define (recur x y n)
	(when
	 (onboard? x y)
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
			      ((< elem n) #f) ;;been here before ..
			      ((= elem n) #f) ;; also been here before
			      ((> elem n) (array-set! arr2 n x y)) ;; new low found
			      ))))))
      ;; find enemies within range with lowest health , then sort by Y axis , then sort X axis
      ;; if no such enemies then go looking for opponent  FOOO
      
      (cond
       (#f 1)       
       ;; ((attack-imminent type x y)	
       ;; ((opponent? type x (- y 1)) attack x y-1)
       ;; ((opponent? type (- x 1) y)  attack x-1 y)
       ;; ((opponent? type (+ x 1) y)  attack x+1 y)
       ;; ((opponent? type x (+ y 1))  attack x (+ y 1))
       (#t ;; go looking for opponent 
	(recur x (- y 1) 1)      
	(recur (- x 1) y 1)
	(recur (+ x 1) y 1)
	(recur x (+ y 1) 1))))
    ;;(map breadth-first (lexicographic arr))
    ;; iterate over lexicographic ordering
    (let loop ((ordering (lexicographic arr))
	       (i 0))
      (cond
       ((null? ordering) 'done)
       (#t (let ((actor (car ordering)))
	     (breadth-first actor)
	     (format #t "~%actor <~a>~%" i)
	     (show-array arr2)
	     (format #t "~%~%")
	     (loop (cdr ordering) (+ i 1))))))))





    

     
