;; -*- geiser-scheme-implementation: guile -*-

#|

development of scheme code writing in nested procedures is both error prone and 

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
(define elf-hit-points 200)
(define elf-power 3)
(define goblin-hit-points 200)
(define goblin-power 3)
;; hit points == health 
(define make-elf (lambda () (list 'elf elf-hit-points elf-power )))
(define make-goblin (lambda () (list 'goblin goblin-hit-points goblin-power)))

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

(define (make-character type health power x y)
  (list type health power x y))

(define (character? x)
  (or (elf? x)
      (goblin? x)
      (pair? x)
      (= (length x) 5)))


;; namespace conflicts character in problem scope and character in programming language
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



(define (find-goblins arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      ;; (map (lambda (p)
      ;; 	     (when (elf? p)
      ;; 	       (match p
      ;; 		 ((goblin n x y)		   
      ;; 		  (format #t "GOBLIN at {~a,~a} is {~a}~%" x y (array-ref arr x y))))))
      ;; 	   gobs)
      gobs)))


(define (find-elfs arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      ;; (map (lambda (p)
      ;; 	     (when (elf? p)
      ;; 	       (match p
      ;; 		 ((elf n x y)		   
      ;; 		  (format #t "ELF at {~a,~a} is {~a}~%" x y (array-ref arr x y))))))
      ;; 	   efs)      
      efs)))



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


(define (char-x x)
  (third x))

(define (char-y y)
  (fourth y))

(define (char-type x)
  (car x))

(define (char-health x)
  (second x))

;; idea now is that the breadth-first search should be started from the opponent and not the player 
;; this way there is only one such shortest back to player - which then identifies what square can reach that

;; foreach : like map except does not accumulate results 
(define (foreach xs f)
  (cond
   ((null? xs) #f)
   (#t (f (car xs))
       (foreach (cdr xs) f))))


;; a global we can inspect - how do inspect a closure ?? how does introducing a global help ??
(define *arr2* #f)


(define (onboard? x y arr)
  (define xlim (array-width arr))
  (define ylim (array-height arr))
  (and (>= x 1) (<= x xlim)
       (>= y 1) (<= y ylim)))



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
  
  
;; (elfs arr)
;; (goblins arr)

(define (lexicographic arr f)
  (array-loop arr f))


(define (lexicographic-players arr)
  (let ((result '()))
    (array-loop arr (lambda (arr x y)
		      (let ((e (array-ref arr x y)))
			(cond
			 ((elf? e) (set! result (cons (list 'elf (second e) x y) result)))
			 ((goblin? e) (set! result (cons (list 'goblin (second e) x y) result)))
			 (#t #f)))))
    (reverse result)))

;; ;; how do i just make a macro without headache of scheme cleanliness 
;; (define-syntax mdir
;;   (syntax-rules ()
;;     ((_ dx dy arr var) 
;;      (let ((dx (- px 1))(dy py))
;;        (when (onboard? dx dy out)
;; 	 (let ((val (array-ref out dx dy)))
;; 	   (when (and (integer? val) (or (not left) (< val left)))
;; 	     (set! left val)))))


(defmacro mdir (x y arr var)
  `(let ((dx ,x)(dy ,y))
     (when (onboard? dx dy ,arr)
       (let ((val (array-ref ,arr dx dy)))
	 (when (and (integer? val) (or (not left) (< val left)))
	   (set! ,var val))))))

;; this-player-goes dir
;; where dir can be up down left nowhere
;; ptype phealth px py
(defmacro this-player-goes (dir)
  `(cond
    ((eq? ',dir 'left) (format #t "LEFT~%") (add-to-results-left))
    ((eq? ',dir 'right) (format #t "RIGHT~%")(add-to-results-right))
    ((eq? ',dir 'down) (format #t "DOWN~%")(add-to-results-down))
    ((eq? ',dir 'up) (format #t "UP~%")(add-to-results-up))
    ((eq? ',dir 'nowhere) (format #t "NOWHERE~%")(add-to-results-nowhere))
    (#t (error "how did i get here?"))))

(defmacro add-to-results-left ()
  `(set! results (cons (list ptype phealth (- px 1) py) results)))

(defmacro add-to-results-right ()
  `(set! results (cons (list ptype phealth (+ px 1) py) results)))

(defmacro add-to-results-down ()
  `(set! results (cons (list ptype phealth px (+ py 1)) results)))

(defmacro add-to-results-up ()
  `(set! results (cons (list ptype phealth px (- py 1)) results)))

(defmacro add-to-results-nowhere ()
  `(set! results (cons (list ptype phealth px py) results)))


(defmacro %move-somewhere% ()
  `(begin
     (foreach opponents
	      (lambda (opponent)
		(format #t "considering ~a vs ~a ~%" player opponent)
		(let ((out (breadth-first-label-caves arr
						      (opponent-x opponent)
						      (opponent-y opponent))))
		  (mdir (- px 1) py out left)
		  (mdir (+ px 1) py out right)
		  (mdir px (- py 1) out up)
		  (mdir px (+ py 1) out down))))
     ;; determine lowest
     (when left (set! dirs (cons `(left ,left) dirs)))
     (when right (set! dirs (cons `(right ,right) dirs)))
     (when up (set! dirs (cons `(up ,up) dirs)))
     (when down (set! dirs (cons `(down ,down) dirs)))
     (format #t "dirs => ~a~%" dirs)
     ;; filter the lowest
     
     (set! sorted-dirs (sort dirs (lambda (x y)(< (second x)(second y)))))
     (format #t "sorted dirs => ~a~%" sorted-dirs)
     (when (not (null? sorted-dirs))
       (let ((lowest-val (second (first sorted-dirs))))
	 (set! min-dirs (filter (lambda (x)(<= (second x) lowest-val)) sorted-dirs))))
     (format #t "min dirs => ~a~%" min-dirs)
     ;; pick the lexicographic direction for this player to move
     
     (cond
      ((assoc 'up min-dirs) (this-player-goes up))
      ((assoc 'left min-dirs) (this-player-goes left))
      ((assoc 'right min-dirs) (this-player-goes right))
      ((assoc 'down min-dirs) (this-player-goes down))
      (#t (this-player-goes nowhere)))))

  

;;
;; * ignore combat for now *
;;
;; for grid {array} find all goblins , find all elfs 
;; for each item {goblin,elf} on grid in lexicographic order - call this player 
;;  the given set of enemies
;;    for each enemy - do breadth-first-label-caves arr at enemy x y position
;;      look at player , look at up / down / left / right directions and note the score
;;      lowest score in that direction gets noted
;;      keep a global score - after each enemy has been considered - we get a global overall best move
;;      situation
;;     player + left right up down
;;   some directions may be false #f because for instance a wall could be in that direction 
;;        
(define (task arr)
  (define goblins #f)
  (define elfs #f)
  (define opponents #f)
  (define opponent-x (lambda (e) (third e)))
  (define opponent-y (lambda (e) (fourth e)))
  (define opponent-health (lambda (e) (second e)))
  (define player-x (lambda (e) (third e)))
  (define player-y (lambda (e) (fourth e)))
  (define player-health (lambda (e) (second e)))
  (define player-type (lambda (e) (first e)))  
  (define players (lexicographic-players arr))
  (define results '())
  (format #t "players=> ~a~%" players)
  
  ;; find elfs and goblins 
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (e g)
      (set! elfs e)
      (set! goblins g)))
  ;; in lexicographic order
  (foreach players
	   (lambda (player)
	     (define phealth (player-health player))
	     (define ptype (player-type player))
	     (define px (player-x player))
	     (define py (player-y player))
	     (format #t "player = ~a : ~a ~a ~a ~a~%" player ptype phealth px py)	     
	     (define left #f)
	     (define right #f)
	     (define down #f)
	     (define up #f)
	     (define dirs '())
	     (define sorted-dirs '())
	     (define min-dirs '())
	     (when (elf? player) (set! opponents goblins))
	     (when (goblin? player) (set! opponents elfs))
	     ;; *ignore combat*
	     (%move-somewhere%)
	     ))
  results)










  



