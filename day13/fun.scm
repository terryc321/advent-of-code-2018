
;; --------------------------------------------------------------------------------

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
(define input (call-with-input-file "input"
		(lambda (port)
		  (read-lines port))))

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


(define solve
  (lambda (step)
    (format #t "step ~a ~%" step)
    (cond
     ((train-collision? trains) (format #t "trains have collided ~%"))
     (#t (set! trains (map train-shunt trains))
	 (solve (+ 1 step))))))



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





(define train-collision?
  (lambda (known)
    (define train-helper (lambda (xs exit)
			   (cond
			    ((null? xs) #f)
			    (#t (let ((x (car xs))
				      (ys (cdr xs)))
				  (do-list (y ys)
					   (cond
					    ((cart-equal-position? x y)
					     ;; ---- message -----
					     (format #t "trains collided ~%")
					     (pp x)
					     (format #t "~%")
					     (pp y)
					     (format #t "~%")
					     
					     (exit #t))))
				  (train-helper (cdr xs) exit))))))
					    
    (call/cc (lambda (exit)
	       (train-helper known exit)
	       #f))))


	       ;; (letrec ((bar (lambda (carts)
	       ;; 		       (format #t "checking BAR .. carts ~A ~%" carts)
	       ;; 		       (cond
	       ;; 			((null? carts) #f)
	       ;; 			(#t (let ((cart (first carts))
	       ;; 				  (others (cdr carts)))
	       ;; 			      ;; see if collision this cart with others
	       ;; 			      (foo cart others)
	       ;; 			      ;; move to next set of carts
	       ;; 			      (bar others))))))
	       ;; 		(foo (lambda (cart carts)
	       ;; 		       (format #t "checking FOO .. cart ~A vs carts ~a ~%" cart carts)
	       ;; 		       (cond
	       ;; 			((null? carts) #f) ;; no collisions here
	       ;; 			(#t (let ((cart-a cart)
	       ;; 				  (cart-b (first carts)))
	       ;; 			      ((cart-equal-position? cart-a cart-b)
	       ;; 			       (exit (list 'collision cart-a cart-b)))
	       ;; 			      (#t (foo cart (cdr carts)))))))))
	       ;; 	 (bar trains)		 
	       ;; 	 #f)))))






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
(define (solver)
  (let ((first-step 1))
    (solve first-step)))



;;(solver)









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


;;input ------------ puzzle ---------------------------
#|
step 423 
trains collided 
((train-no 11) (x 124) (y 90) (direction up) (internal left))
               ^^^^^^^^^^^^^^^

((train-no 8) (x 124) (y 90) (direction down) (internal ahead))
              ^^^^^^^^^^^^^^
trains have collided 

with ZERO based indexing  x , y of first crash is 124 , 90

124,90      ACCEPTED ANSWER



|#
