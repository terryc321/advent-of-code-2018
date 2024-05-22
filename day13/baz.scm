
;; --------------------------------------------------------------------------------
;; BAZ .scm


(import scheme)

(import (simple-loops))

(import (chicken io))
(import (chicken pretty-print))

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))

(import procedural-macros)
(import regex)

(import simple-md5)

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
(define output #f)

;;(define input (call-with-input-file "crash1.txt" (lambda (port) (read-lines port))))
(define input (call-with-input-file "input" (lambda (port) (read-lines port))))

(define *tick* 0)
(define train-grid #f)
(define trains #f)

(define decode-track
  (lambda ()
    (let ((train-no 0)
	  (internal 'left))
      (set! train-grid (make-hash-table))
      (set! trains '())
      (do-list (py (iota (length input)))
	       (do-list (px (iota (string-length (car input))))
			(let* ((x px)
			       (y py)
			       (ch (string-ref (list-ref input py) px)))

			  ;; give trains a unique number 
			  (when (member ch '( #\v #\^ #\< #\> ))
			    (set! train-no (+ 1 train-no)))
			  
			  ;; 
			  ;;(format #t "~a,~a -> ~a ~%" x y ch)
			  ;; decode track
			  (cond
			   ;;
			   ((char=? ch #\+)
			    (hash-table-set! train-grid (list x y) 'cross )
			    #t)			 
			   ;; 
			   ((char=? ch #\/)
			    (hash-table-set! train-grid (list x y) 'slash )
			    #t)
			   ;; 
			   ((char=? ch #\\)
			    (hash-table-set! train-grid (list x y) 'backslash )		  
			    #t)
			   ;; train vertical up
			   ((char=? ch #\^)
			    (set! trains (cons `((train-no ,train-no)
						 (x ,x)
						 (y ,y)
						 (direction up)
						 (internal ,internal))
					       trains))
			    (hash-table-set! train-grid (list x y) 'vert))
			   ;; train vertical down 
			   ((char=? ch #\v)
			    (set! trains (cons `((train-no ,train-no)
						 (x ,x)
						 (y ,y)
						 (direction down)
						 (internal ,internal))
					       trains))
			    (hash-table-set! train-grid (list x y) 'vert))
			   
			   ;; vertical track
			   ((char=? ch #\| )
			    (hash-table-set! train-grid (list x y) 'vert))

			   ;; right train
			   ((char=? ch #\>)
			    (set! trains (cons `((train-no ,train-no)
						 (x ,x)
						 (y ,y)
						 (direction right)
						 (internal ,internal))
					       trains))			    
			    (hash-table-set! train-grid (list x y) 'horz))
			   ;; left train
			   ((char=? ch #\<)
			    (set! trains (cons `((train-no ,train-no)
						 (x ,x)
						 (y ,y)
						 (direction left)
						 (internal ,internal))
					       trains))
			    
			    (hash-table-set! train-grid (list x y) 'horz))
			   ;; horz track
			   ((char=? ch #\-)
			    (hash-table-set! train-grid (list x y) 'horz))
			   ;; empty space , hopefully no tabs ?
			   ((char=? ch #\space)
			    #f)
			   (#t
			    (error (list 'unknown-character ch x y))))))))))





(define grid
  (lambda (x y)
    (hash-table-ref/default train-grid (list x y) #f)))



;; make the track then ...
(decode-track)


;; train can hit + cause it to do silly
;; train can hit /
;; train can hit \
;; 

       
       ;; 	(cond
       ;; 	 ((eq? (grid (- x 1) y) 'backslash) ;; \
       ;; 	  state ;;(list (- x 1) (- y 1) 'up internal)
       ;; 	  )
       ;; 	 (#t
       ;; 	  state)))
       ;; ((eq? direction 'right)
       ;; 	`((train-no ,train-no) (x ,x) (y ,y) (direction ,direction) (internal ,internal)))
       ;; ((eq? direction 'up)
       ;; 	`((train-no ,train-no) (x ,x) (y ,y) (direction ,direction) (internal ,internal)))
       ;; ((eq? direction 'down)
       ;; 	`((train-no ,train-no) (x ,x) (y ,y) (direction ,direction) (internal ,internal)))


;; UP means - y 1
;; DOWN means + y 1
;; LEFT means - x 1
;; RIGHT means + x 1 

;; ----------- trains meet crossing
;; internal 0 = go left ... left viz train , or left from view observer looking down ?
;; internal 1 = go ahead
;; internal 2 = go right

(define nx
  (lambda (dir)
    (cond
     ((eq? dir 'left) 'ahead)
     ((eq? dir 'ahead) 'right)
     ((eq? dir 'right) 'left)
     (#t (error (list 'nx 'bad 'direction dir))))))



;; ----------------------------- ok x - 1 ----------------------------
(define train-shunt-left
  (lambda (state at)
    (let* ((train-no (second (assoc 'train-no state)))
	   (x (second (assoc 'x state)))
	   (y (second (assoc 'y state)))
	   (direction (second (assoc 'direction state)))
	   (internal (second (assoc 'internal state))))
      (cond
       ;; 
       ((eq? at 'backslash) ;; \ go up
	`((train-no ,train-no) (x ,x) (y ,(- y 1)) (direction up) (internal ,internal)))
       ;; 
       ((eq? at 'slash) ;; / go down
	`((train-no ,train-no) (x ,x) (y ,(+ y 1)) (direction down) (internal ,internal)))
       ;; crossing
       ((eq? at 'cross)
	;;'train-shunt-left-cross-not-implemented
	(cond
	 ((eq? internal 'left) ; down 
	  `((train-no ,train-no) (x ,x) (y ,(+ 1 y)) (direction down) (internal ,(nx internal))))
	 ((eq? internal 'ahead) ; left 
	  `((train-no ,train-no) (x ,(- x 1)) (y ,y) (direction left) (internal ,(nx internal))))
	 ((eq? internal 'right) ; up  
	  `((train-no ,train-no) (x ,x) (y ,(+ -1 y)) (direction up) (internal ,(nx internal))))
	 (error (list 'train-shunt-left at 'crossing 'bad 'internal))))
       ;; horz
       ((eq? at 'horz)
	`((train-no ,train-no) (x ,(- x 1)) (y ,y) (direction ,direction) (internal ,internal)))
       (#t (error (list 'unknown-item at 'train-shunt-left state)))))))


 




;;-----------------------------ok x + 1 --------------------       
    
(define train-shunt-right
  (lambda (state at)
    (let* ((train-no (second (assoc 'train-no state)))
	   (x (second (assoc 'x state)))
	   (y (second (assoc 'y state)))
	   (direction (second (assoc 'direction state)))
	   (internal (second (assoc 'internal state))))
      (cond
       ;;
       ((eq? at 'backslash) ;; \ go down
	`((train-no ,train-no) (x ,x) (y ,(+ y 1)) (direction down) (internal ,internal)))
       ;;
       ((eq? at 'slash) ;; / go up
	`((train-no ,train-no) (x ,x) (y ,(- y 1)) (direction up) (internal ,internal)))
       ;; cross
       ((eq? at 'cross)
	;;'train-shunt-right-cross-not-implemented
	(cond
	 ((eq? internal 'left) ; up
	  `((train-no ,train-no) (x ,x) (y ,(+ -1 y)) (direction up) (internal ,(nx internal))))
	 ((eq? internal 'ahead) ; right 
	  `((train-no ,train-no) (x ,(+ x 1)) (y ,y) (direction right) (internal ,(nx internal))))
	 ((eq? internal 'right) ; down 
	  `((train-no ,train-no) (x ,x) (y ,(+ 1 y)) (direction down) (internal ,(nx internal))))
	 (error (list 'train-shunt-right at 'crossing 'bad 'internal))))
       ;; horz
       ((eq? at 'horz)
	`((train-no ,train-no) (x ,(+ x 1)) (y ,y) (direction ,direction) (internal ,internal)))
       (#t (error (list 'unknown-item at 'train-shunt-right state)))))))



;;----------------------------up  - y 1---------------------
(define train-shunt-up
  (lambda (state at)
    (let* ((train-no (second (assoc 'train-no state)))
	  (x (second (assoc 'x state)))
	  (y (second (assoc 'y state)))
	  (direction (second (assoc 'direction state)))
	  (internal (second (assoc 'internal state))))
      (cond
       ;; back slash
       ((eq? at 'backslash) ;; \ go left
	`((train-no ,train-no) (x ,(- x 1)) (y ,y) (direction left) (internal ,internal)))

       ;; slash
       ((eq? at 'slash) ;; / go right
	`((train-no ,train-no) (x ,(+ x 1)) (y ,y) (direction right) (internal ,internal)))       

       ;; cross
       ((eq? at 'cross)
	;;'train-shunt-up-cross-not-implemented
	(cond
	 ((eq? internal 'left) ; left
	  `((train-no ,train-no) (x ,(+ -1 x)) (y ,y) (direction left) (internal ,(nx internal))))
	 ((eq? internal 'ahead) ; up 
	  `((train-no ,train-no) (x ,x) (y ,(+ -1 y)) (direction up) (internal ,(nx internal))))
	 
	 ((eq? internal 'right) ; right
	  `((train-no ,train-no) (x ,(+ x 1)) (y ,y) (direction right) (internal ,(nx internal))))
	 
	 (error (list 'train-shunt-up at 'crossing 'bad 'internal))))
       	
       ;; vert
       ((eq? at 'vert)
	`((train-no ,train-no) (x ,x) (y ,(- y 1)) (direction ,direction) (internal ,internal)))      
       (#t (error (list 'unknown-item at 'train-shunt-up state)))))))



;; ----------------------down + y 1 ----------------------------------

(define train-shunt-down
  (lambda (state at)
    (let* ((train-no (second (assoc 'train-no state)))
	  (x (second (assoc 'x state)))
	  (y (second (assoc 'y state)))
	  (direction (second (assoc 'direction state)))
	  (internal (second (assoc 'internal state))))
      (cond
       ((eq? at 'backslash) ;; \ go right
	`((train-no ,train-no) (x ,(+ x 1)) (y ,y) (direction right) (internal ,internal)))
       ((eq? at 'slash) ;; / go left
	`((train-no ,train-no) (x ,(- x 1)) (y ,y) (direction left) (internal ,internal)))       
       ;; cross
       ((eq? at 'cross)
	;;'train-shunt-down-cross-not-implemented
	(cond
	 ((eq? internal 'left) ; right
	  `((train-no ,train-no) (x ,(+ x 1)) (y ,y) (direction right) (internal ,(nx internal))))
	 
	 ((eq? internal 'ahead) ;  down
	  `((train-no ,train-no) (x ,x) (y ,(+ 1 y)) (direction down) (internal ,(nx internal))))
	 
	 ((eq? internal 'right) ;  left
	  `((train-no ,train-no) (x ,(- x 1)) (y ,y) (direction left) (internal ,(nx internal))))
	 
	 (error (list 'train-shunt-down at 'crossing 'bad 'internal))))
       ;; vert
       ((eq? at 'vert)
	`((train-no ,train-no) (x ,x) (y ,(+ y 1)) (direction ,direction) (internal ,internal)))       
       (#t (error (list 'unknown-item at 'train-shunt-down state)))))))

       


(define train-shunt
  (lambda (state)
    (let* ((direction (second (assoc 'direction state)))
	   (x (second (assoc 'x state)))
	   (y (second (assoc 'y state)))
	   (at (grid x y)))
      (cond
       ((eq? direction 'left) (train-shunt-left state at))
       ((eq? direction 'up) (train-shunt-up state at))
       ((eq? direction 'right) (train-shunt-right state at))
       ((eq? direction 'down) (train-shunt-down state at))
       (#t 
        (error (list 'unknown-direction 'train-shunt)))))))


(define (remove-trains xs na nb)
  (let ((res '()))
    (do-list (x xs)
	     (let ((n (second (assoc 'train-no x))))
	       (cond
		((or (= na n) (= nb n)) #f)
		(#t (set! res (cons x res))))))
    res))




#|

do carts occupy same square ?
x1 = x2 and y1 = y2

|#
(define cart-equal-position?
  (lambda (a b)
    (let ((ax (second (assoc 'x a)))
	  (ay (second (assoc 'y a)))
	  (bx (second (assoc 'x b)))
	  (by (second (assoc 'y b))))
      (let ((outcome (and (= ax bx) (= ay by))))
	(if outcome
	    (begin
	      ;;(format #t "carts collided ~a ~a : ~a ~a ~%" ax ay bx by)
	      #t
	      )
	    #f)))))


#|

do carts appear to be on collision course ?
if cart 

((train-no 15) (x 77) (y 27) (direction right) (internal ahead))

a up       b up down left right
a down     b up down left right
a left     b up down left right
a right    b up down left right

|#
(define cart-will-collide?
  (lambda (a b)
    (match a
      (
       (('train-no ano)('x ax)('y ay)('direction adir)('internal aint))
       (match b
	 (
	  (('train-no bno)('x bx)('y by)('direction bdir)('internal bint))

	  (let ((fax ax)
		(fay ay)
		(fbx bx)
		(fby by))

	    ;; figure out future position of train a			
	    (cond
	     ((eq? adir 'left) (set! fax (+ -1 fax)))
	     ((eq? adir 'right) (set! fax (+ +1 fax)))
	     ((eq? adir 'up) (set! fay (+ -1 fay)))
	     ((eq? adir 'down) (set! fay (+ +1 fay))))
	    
	    ;; figure out future position of train b
	    (cond
	     ((eq? bdir 'left) (set! fbx (+ -1 fbx)))
	     ((eq? bdir 'right) (set! fbx (+ +1 fbx)))
	     ((eq? bdir 'up) (set! fby (+ -1 fby)))
	     ((eq? bdir 'down) (set! fby (+ +1 fby))))

	    ;; collision condition is future case of 
	    (or
	     ;; horz collision
	     (and (= (+ ax  1)  bx) (= ay by) (eq? adir 'right) (eq? bdir 'left))
	     (and (= (- ax  1) bx) (= ay by) (eq? adir 'left) (eq? bdir 'right))
	     
	     ;; vert collision
	     (and (= ax bx) (= (+ ay 1) by)  (eq? adir 'down) (eq? bdir 'up))
	     (and (= ax bx) (= (- ay 1) by)  (eq? adir 'up) (eq? bdir 'down))
	     
	     (and (= fax fbx) (= fay fby))
	    )))
	 (_ (error "cart-will-collide? : no match train b"))))
      (_ (error "cart-will-collide? : no match train a")))))





#|
> (solver)
((train-no 12) (x 69) (y 93) (direction right) (internal right))
 69,93 


> (solver)
((train-no 13) (x 96) (y 92) (direction down) (internal ahead))
 96,92  is NOT the ACCEPTED answer ...
because collision detection is squiff

collision simple if sat on same square 

                   A _ B
                  _  AB _


problem with collision detection if two spaces between them , collision is NOT detected !
ouch

                _A_ _ _ _B_        
                  _A_ _B_
                  _B_ _A_
                _B_ _ _ _A_        

also pre-emptive collision detection may throw off the computation for final result



|#


(define train-collision?
  (lambda (known)
    (define train-helper (lambda (xs exit)
			   (cond
			    ((null? xs) #f)
			    (#t (let ((x (car xs))
				      (ys (cdr xs)))
				  (do-list (y ys)
					   (cond
					    ((cart-will-collide? x y)
					     
					     ;; pre-emptive train collision detection
					     (format #t "pre-emptive collision ~%")
					     (pp x)
					     (format #t "~%")
					     (pp y)
					     (format #t "~%")
					     
					     (exit (list x y)))
					    ((cart-equal-position? x y)
					     ;; ;; ---- message -----
					     (format #t "trains collided ~%")
					     (pp x)
					     (format #t "~%")
					     (pp y)
					     (format #t "~%")
					     (exit (list x y)))
					    
					    ))
				  (train-helper (cdr xs) exit))))))
					    
    (call/cc (lambda (exit)
	       (train-helper known exit)
	       #f))))


(define (check-train-collision)
  (list
   (train-collision? '(
		       ((train-no 2) (x 74) (y 1) (direction left) (internal 0))
		       ((train-no 1) (x 74) (y 1) (direction left) (internal 0))
		       ))
   (train-collision? '(
		       ((train-no 2) (x 75) (y 1) (direction left) (internal 0))
		       ((train-no 1) (x 74) (y 1) (direction left) (internal 0))
		       ))
   (train-collision? '(
		       ((train-no 1) (x 74) (y 1) (direction left) (internal 0))
		       ))
		     
   ))


;; ------------- solver ----------------------

(define solve
  (lambda (step)
    ;;(format #t "step ~a ~%" step)
    ;; (format #t "trains ~%")
    ;; (pp trains)

    (format output "(tick ~a)~%" *tick*)
    (set! *tick* (+ 1 *tick*))
    (do-list (train trains)
	     (format output "~a~%" train))

    
    ;; (format #t "~%")
    ;; (show-track)
    
    (let ((crash (train-collision? trains)))
      (cond
       ((pair? crash)
	(let ((a (car crash))
	      (b (cadr crash)))
	  (let* ((na (second (assoc 'train-no a)))
		(nb (second (assoc 'train-no b)))
		(len-before (length trains))
		(len-after (- len-before 2)))
	    (set! trains (remove-trains trains na nb))
	    ;; only remove 2 trains 
	    (assert (= len-after (length trains)))
	    (cond
	     ((null? (cdr trains)) (car trains))
	     (#t
	      ;; remove any other collisions before we move trains
	      ;; (set! trains (map train-shunt trains))
	      (solve (+ step 1)))))))
       (#t
	(set! trains (map train-shunt trains))

	;; is any train on a corner ?
	;; doesnt matter , carts can be on corners - so red herring	
	(solve (+ step 1)))))))




(define (solver)
  (call-with-output-file "output"
    (lambda (port)
      (set! output port)

      ;; display board
      (format output "(begin-track)~%")
      (do-list (py (iota (length input)))
	       (do-list (px (iota (string-length (car input))))
			(let* ((x px)
			       (y py)
			       (at (grid x y)))
			  (cond
			   ((not at)  ;; common lisp does not understand #f 
			    (format output "(track ~a ~a nil)~%" x y at))
			   (#t
			    (format output "(track ~a ~a ~a)~%" x y at))))))
      (format output "(end-track)~%")
      (let ((first-step 1))
	(solve first-step)))))






;;(solver)

;; how big is the track
(define (show-track)
  (format #t "~%~%")
  (do-list (py (iota (length input)))
	   (do-list (px (iota (string-length (car input))))
		    (let* ((x px)
			   (y py)
			   (at (grid x y))
			   (dir "^"))

		       
		      (let ((count 0))	   
			(do-list (train trains)
				 (let* ((train-no (second (assoc 'train-no train)))
					(tx (second (assoc 'x train)))
					(ty (second (assoc 'y train)))
					(direction (second (assoc 'direction train)))
					(internal (second (assoc 'internal train))))
				   (when
				       (and (= x tx) (= y ty))
				     (set! count (+ count 1))
				     (cond
				      ((eq? direction 'up) (set! dir "^"))
				      ((eq? direction 'left) (set! dir "<"))
				      ((eq? direction 'right) (set! dir ">"))
				      ((eq? direction 'down) (set! dir "v"))))
				   ))
			
		      (cond
		       ((< count 1)
			(cond
			 ((eq? at 'cross) (format #t "+"))
			 ((eq? at 'slash) (format #t "/"))
			 ((eq? at 'backslash) (format #t "\\"))
			 ((eq? at 'horz) (format #t "-"))
			 ((eq? at 'vert) (format #t "|"))
			 ((eq? at #f) (format #t " "))		       
			 ))
		       ((= count 1) (format #t "~a" dir))
		       ((= count 2) (format #t "*"))
		       ((> count 2) (format #t "?"))
		       )

		      (format #t " ")
		      
		      ;;(format #t " , ")
		      )))
	   (format #t "~%")))







		      
			       

  







;; ---------------- just want a 2d grid then can process


#|


v
^ also means track is | vertical at this position

< also means track is - horizontal at this position 
>

carts are only ever on vertical tracks

+ is an intersection of track



/
\



figure out what type of corner it is
does the train stop on a corner ?

how many trains are there ?

can we visualise the solution ?

step it forward , step it backwards , speed it up n steps ?

track itself does not change

only have train positions



|#


;; 
;;(iter)


#|

;; -------------------- part I  --- SOLVED --------------------------

step 423 
trains collided 
((train-no 11) (x 124) (y 90) (direction up) (internal left))
               ^^^^^^^^^^^^^^^

((train-no 8) (x 124) (y 90) (direction down) (internal ahead))
              ^^^^^^^^^^^^^^
trains have collided 

with ZERO based indexing  x , y of first crash is 124 , 90

124,90      ACCEPTED ANSWER

;; ----------------- part I I ------------------------------

> (solver)

((train-no 13) (x 116) (y 98) (direction down) (internal left))

116,98  .............. FAIL ....... huh ??


|#



