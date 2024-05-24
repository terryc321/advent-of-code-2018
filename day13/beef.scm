
;; --------------------------------------------------------------------------------
;; BEEF.scm 


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


(define make-train
  (lambda (no x y dir)
    (let ((int 'left)
	  (old-x x)
	  (old-y y))
      (lambda (op)
	(cond
	 ((eq? op 'state) `((train-no ,no)(x ,x)(y ,y)(direction ,dir)(internal ,int)(old-x ,old-x)(old-y ,old-y)))
	 ((eq? op 'tick)
	  (set! old-x x)
	  (set! old-y y)
	  (let ((at (grid x y)))
	    (cond
	     ((eq? at 'horz)
	      (cond
	       ((eq? dir 'left) (set! dir 'left) (set! x (- x 1)))
	       ((eq? dir 'right) (set! dir 'right) (set! x (+ x 1)))
	       (#t (error (list "train : horz - bad dir combo " 'train-no no 'x x 'y y 'dir dir 'int int)))))

	     ;; verticals 
	     ((eq? at 'vert)
	      (cond
	       ((eq? dir 'up) (set! dir 'up) (set! y (- y 1)))
	       ((eq? dir 'down) (set! dir 'down) (set! y (+ y 1)))
	       (#t (error (list "train : horz - bad dir combo " 'train-no no 'x x 'y y 'dir dir 'int int)))))

	     ;; slash  / 
	     ((eq? at 'slash)
	      (cond
	       ((eq? dir 'up) (set! dir 'right) (set! x (+ x 1)))
	       ((eq? dir 'down) (set! dir 'left) (set! x (- x 1)))
	       ((eq? dir 'left) (set! dir 'down) (set! y (+ y 1)))
	       ((eq? dir 'right) (set! dir 'up) (set! y (- y 1)))
	       (#t (error (list "train : slash - bad dir " 'train-no no 'x x 'y y 'dir dir 'int int)))))
	     
	     ;; backslash \
	     ((eq? at 'backslash)
	      (cond
	       ((eq? dir 'up) (set! dir 'left) (set! x (- x 1)))
	       ((eq? dir 'down) (set! dir 'right) (set! x (+ x 1)))
	       ((eq? dir 'left) (set! dir 'up) (set! y (- y 1)))
	       ((eq? dir 'right) (set! dir 'down) (set! y (+ y 1)))
	       (#t (error (list "train : backslash - bad dir " 'train-no no 'x x 'y y 'dir dir 'int int)))))
	       	     	     
	     ;; cross 
	     ((eq? at 'cross)
	      (cond
	       ((and (eq? dir 'left) (eq? int 'left)) (set! dir 'down) (set! y (+ y 1)))
	       ((and (eq? dir 'left) (eq? int 'right)) (set! dir 'up) (set! y (- y 1)))
	       ((and (eq? dir 'left) (eq? int 'ahead)) (set! dir 'left) (set! x (- x 1)))
	       
	       ((and (eq? dir 'right) (eq? int 'left)) (set! dir 'up) (set! y (- y 1)))
	       ((and (eq? dir 'right) (eq? int 'right)) (set! dir 'down) (set! y (+ y 1)))
	       ((and (eq? dir 'right) (eq? int 'ahead)) (set! dir 'right) (set! x (+ x 1)))

	       ((and (eq? dir 'up) (eq? int 'left)) (set! dir 'left) (set! x (- x 1)))
	       ((and (eq? dir 'up) (eq? int 'right)) (set! dir 'right) (set! x (+ x 1)))
	       ((and (eq? dir 'up) (eq? int 'ahead)) (set! dir 'up) (set! y (- y 1)))

	       ((and (eq? dir 'down) (eq? int 'left)) (set! dir 'right) (set! x (+ x 1)))
	       ((and (eq? dir 'down) (eq? int 'right)) (set! dir 'left) (set! x (- x 1)))
	       ((and (eq? dir 'down) (eq? int 'ahead)) (set! dir 'down) (set! y (+ y 1)))

	       (#t (error (list "train : cross bad dir int combo " 'train-no no 'x x 'y y 'dir dir 'int int))))
	      ;; next internal state left -> ahead -> right -> .... rinse n repeat 
	      (set! int (nx int)))

	     ;; unrecognised square
	     (#t
	      (error (list "train : dont know this square type " 'type at 'train-no no 'x x 'y y 'dir dir 'int int)))
	     ;; 
	     ))))))))


(define initial-trains
  (lambda ()
  (map (lambda (x)
	 (match x
	   ( (('train-no no)('x x)('y y)('direction dir)('internal int))
	     (make-train no x y dir)
	     )))
        '(((train-no 1) (x 73) (y 0) (direction left) (internal left))
	  ((train-no 6) (x 134) (y 43) (direction left) (internal left))
	  ((train-no 7) (x 38) (y 57) (direction left) (internal left))
	  ((train-no 11) (x 31) (y 87) (direction left) (internal left))
	  ((train-no 13) (x 85) (y 97) (direction left) (internal left))
	  ((train-no 17) (x 107) (y 133) (direction left) (internal left))
	  ((train-no 2) (x 77) (y 13) (direction right) (internal left))
	  ((train-no 4) (x 40) (y 39) (direction right) (internal left))
	  ((train-no 5) (x 71) (y 42) (direction right) (internal left))
	  ((train-no 9) (x 98) (y 66) (direction right) (internal left))
	  ((train-no 15) (x 78) (y 116) (direction right) (internal left))
	  ((train-no 3) (x 66) (y 20) (direction down) (internal left))
	  ((train-no 10) (x 49) (y 80) (direction down) (internal left))
	  ((train-no 14) (x 6) (y 107) (direction down) (internal left))
	  ((train-no 16) (x 40) (y 131) (direction down) (internal left))
	  ((train-no 8) (x 143) (y 59) (direction up) (internal left))
	  ((train-no 12) (x 24) (y 96) (direction up) (internal left))))))



 
(define remove-train
  (lambda (n xs)
    (let ((ys '()))
      (do-list (fn xs)
	      (cond
	       ((= (second (assoc 'train-no (fn 'state))) n)
		#f ;; ignore it
		)
	       (#t (set! ys (cons fn ys)))))
      ys)))



;; aka find-collision
(define remove-crashed-trains
  (lambda (trains)
    (let ((collisions '())
	  (remove-list '()))
      (do-list (train-a trains)
	       (do-list (train-b trains)
			(match (train-a 'state)
			  ( (('train-no ano)
			     ('x ax)
			     ('y ay)
			     ('direction adir)
			     ('internal aint)
			     ('old-x aox)
			     ('old-y aoy))
			    (match (train-b 'state)
			      ( (('train-no bno)
				 ('x bx)
				 ('y by)
				 ('direction bdir)
				 ('internal bint)
				 ('old-x box)
				 ('old-y boy))

				;; ensure compare different trains
				(cond
				 ((not (= ano bno))
				  (cond
				   ((and (= ax bx) (= ay by))
				    (format #t "(tick ~a)~%" *tick*)
				    (format #t "collision conv : ~a ~a ! ~%" (train-a 'state) (train-b 'state))
				    (set! collisions (cons (list train-a train-b) collisions))
				    (set! remove-list (cons train-a remove-list))
				    (set! remove-list (cons train-b remove-list))
				    )
				   
				   ((and (= ax box) (= ay boy) (= bx aox) (= by aoy))
				    (format #t "(tick ~a)~%" *tick*)
				    (format #t "collision pass : ~a ~a !~%" (train-a 'state) (train-b 'state))				    (set! remove-list (cons train-a remove-list))
				    (set! remove-list (cons train-b remove-list))			
				    (set! collisions (cons (list train-a train-b) collisions)))				   
				   ))))
				(_ (error "find-collision - no match b"))))
			  (_ (error "find-collision - no match a")))
			))
      
      (do-list (train remove-list)
	       (match (train 'state)
		 ( (('train-no no)('x x)('y y)('direction dir)('internal int)('old-x ox)('old-y oy))
		   ;; remove train no from trains list
		   (set! trains (remove-train no trains))
		   )
		 ( _ (error "find-collision - no match remove list "))))

      ;; how many trains remaining ...
      (when (not (null? collisions))
	(format #t "trains remaining ~a ~%" (length trains)))
      
      trains)))







	     
    

#|
think of a collision like a snake tail
if both sitting on the others snake then there has been a collision
timing is also going to be correct , so no worries there regards tick 

|#



(define solver
  (lambda ()
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
	
	;; simple loop ...
	(let ((trains (initial-trains))
	      (step 0)
	      (ncount 2)
	      (before '())
	      (after '()))
	  (letrec ((foo (lambda () ;; loop
			  ;;(format #t "step ~a ~%" step)

			  (when (= step 32942)
			    (do-list (train trains)
				     (format #t "[32942 : ] ~a~%" (train 'state))))
			  
			  
			  (cond
			   ((and (< ncount 1) (= (length trains) 1)) ((car trains) 'state))
			   (#t

			    ;; one more to see collision disappear
			    (when (= (length trains) 1)
			      (set! ncount (- ncount 1)))
			    
			  (format output "(tick ~a)~%" *tick*)
			  (set! *tick* (+ 1 *tick*))
			  (do-list (train trains)
				   (format output "~a~%" (train 'state)))

			  ;; make each train move a step 
			  (do-list (train trains) (train 'tick))

			  ;; remove the crashed trains
			  (set! trains (remove-crashed-trains trains))

			  (set! step (+ step 1))
			  (foo))))))
	    (foo)))))))


#|
ncount = 1 
((train-no 15) (x 132) (y 123) (direction right) (internal right) (old-x 131) (old-y 123))

ncount = 0
((train-no 15) (x 131) (y 123) (direction right) (internal right) (old-x 130) (old-y 123))

[32942 : ] ((train-no 15) (x 131) (y 123) (direction right) (internal right) (old-x 130) (old-y 123))
((train-no 15) (x 133) (y 123) (direction right) (internal right) (old-x 132) (old-y 123))

131,123  .... REJECTED .....



|#






#|

>(solve)

trains remaining 1 
((train-no 15) (x 131) (y 123) (direction right) (internal right) (old-x 130) (old-y 123))

 131,123

|#





#|


from visualizer

CAIRO-SDL> (gethash 0 *train-history* )


 (((TRAIN-NO 1) (X 73) (Y 0) (DIRECTION LEFT) (INTERNAL LEFT))
 ((TRAIN-NO 6) (X 134) (Y 43) (DIRECTION LEFT) (INTERNAL LEFT))
 ((TRAIN-NO 7) (X 38) (Y 57) (DIRECTION LEFT) (INTERNAL LEFT))
 ((TRAIN-NO 11) (X 31) (Y 87) (DIRECTION LEFT) (INTERNAL LEFT))
 ((TRAIN-NO 13) (X 85) (Y 97) (DIRECTION LEFT) (INTERNAL LEFT))
 ((TRAIN-NO 17) (X 107) (Y 133) (DIRECTION LEFT) (INTERNAL LEFT)))

 ((TRAIN-NO 2) (X 77) (Y 13) (DIRECTION RIGHT) (INTERNAL LEFT))
 ((TRAIN-NO 4) (X 40) (Y 39) (DIRECTION RIGHT) (INTERNAL LEFT))
 ((TRAIN-NO 5) (X 71) (Y 42) (DIRECTION RIGHT) (INTERNAL LEFT))
 ((TRAIN-NO 9) (X 98) (Y 66) (DIRECTION RIGHT) (INTERNAL LEFT))
 ((TRAIN-NO 15) (X 78) (Y 116) (DIRECTION RIGHT) (INTERNAL LEFT))

 ((TRAIN-NO 3) (X 66) (Y 20) (DIRECTION DOWN) (INTERNAL LEFT))
 ((TRAIN-NO 10) (X 49) (Y 80) (DIRECTION DOWN) (INTERNAL LEFT))
 ((TRAIN-NO 14) (X 6) (Y 107) (DIRECTION DOWN) (INTERNAL LEFT))
 ((TRAIN-NO 16) (X 40) (Y 131) (DIRECTION DOWN) (INTERNAL LEFT))

 ((TRAIN-NO 8) (X 143) (Y 59) (DIRECTION UP) (INTERNAL LEFT))
 ((TRAIN-NO 12) (X 24) (Y 96) (DIRECTION UP) (INTERNAL LEFT))
T

6 matches for "<" in buffer: input
      1:-|/\                  /---------------------\           /----------------<---------------------------------------------------------------------\      
     44:|| |||| | | |||  | |    |||  || |  | /-++-----+++--+--+++++-+-+--+++--+-+++-+++--+++-+---+-++-+++-+--++-+-+-+-+--++-+--+---+++--+--++-<-++-++-++\||   
     58:||  || || | |||  | | /+++++--++-+--+++<+++----++---++-++++-----+-+-+----+-+--+----++++---++++-++++---++-++--+\|  ||  |||   ||| || |||| ||| |  ||||||  
     88:    |||||| |  | || |||||| |\+-+<+-++---++--++-+--+-+--+-++-++----+---+---++-++--+-++--+++-++--+++---++++-++--++/||| ||| |  |||| |||| |  |||| ||| | |  
     98:  /-++++++-+--++++\||| || | |   | |    |||  | |    |  | || ||   ||   |   \+-++----++-<++/|| | ||    |||| ||  ||  || |||||  |||| |  | |   ||| ||| | |  
    134:  |    || | |   |  \+-+--------+--------+---+---------------++--+----+-+----+------+--+---+--------+--+----<+-+---------++-+-/         ||||  |        


5 matches for ">" in buffer: input
     14: | |   |    /-+-+----++--+------+-------+---+-+-+-+----++----++---+-----++--->+--------+----+-+-+----+---++-+-----\  |       |         ||      |   |  
     40:|| ||||\+-+-+++--++++---+++--++-+--+---+>-----+++-/|  ||||| | |  |||\-+-++++-++--+++-+---+-++-+++-+--++-+-+-+----++-+--+---+-+-++--++---++-++-++-++/  
     43:|| |||| | | |||  | |\---+++--++-+--+---++-----+++--+--+++++-+-+--+++--+>+++-+++--+++-+---+-++-+++-+--++-+-+-+-+--++-+--+---+++-/|  ||   || || || ||   
     67:||  \++++-+--++--+-+++++-++--+/ |  | | ||| /--++---++-++++-----+-+-+----+-+--+----++-+++-++++-++++>--+---+--+++--+---+++---+++--+\||||  || |  |||| |  
    117:      ||||| | | || |||      \--+--+-+----+--+--------++--+-++---++---++---+-+->----+--++-++----+----+-++++/  ||   | |  |||   || |         || |     || 


2 matches for "\^" in buffer: input
     60:||  || || | |||  | | |||\++--++-+--+-+-+++----++---++-++++-----+-+-+----+-+--+----++++---++++-++++---++-/|  |||  ||  |||   |||  | |||| ||| |  |^||||  
     97:    |||||| |  |||| ||| |^ | |   | |    |||  | |    |  | || ||   ||   |   || ||    ||  |||/+-+-++----++++-++--++--++-+++\|  |||| |  | |   ||| ||| | |  

4 matches for "v" in buffer: input
     21: | |  ||    ||| ||  |\+--++-----+-------/   | | | |    |||| |||   v|  | ||    |  | |   |    | | | |  |/--++-+----++--+-------+-++---+--++--+---+--\|  
     81:|    |||| \---+--+-+++++---+-++-+--+-+-++--++-+/ v |  |\++-+-----+---+---++--+--+-++-++++-++--+++---+++--+--/||||||  |||   |||| |||| |  |||| ||||| |  
    108:  |  |v|||/----+++++++-+----+--\  | |   || ||      \-++--+-++---++---++--++-++-+------++-++-+--+----+-++-++--++---+-++-++--+-++-+--/|    ||| |     || 
    132:  |    |||| |   |  || |        |  |     v   |               ||  |    | |    |      |  |  \+--------++-+-----+-+---+----/|| | |         ||||  |        


(+ 6 5 2 4 )  17 trains

|#







#|

latest collision sheet
    422     457    471     492    1023    1324    2464    32942

--------------------------------------------------------------------------

(tick 422)
collision conv : ((train-no 11) (x 124) (y 90) (direction up) (internal left) (old-x 124) (old-y 91)) ((train-no 8) (x 124) (y 90) (direction down) (internal ahead) (old-x 124) (old-y 89)) ! 

(tick 422)
collision conv : ((train-no 8) (x 124) (y 90) (direction down) (internal ahead) (old-x 124) (old-y 89)) ((train-no 11) (x 124) (y 90) (direction up) (internal left) (old-x 124) (old-y 91)) ! 
trains remaining 15 

(tick 457)
collision pass : ((train-no 2) (x 57) (y 118) (direction up) (internal ahead) (old-x 57) (old-y 119)) ((train-no 16) (x 57) (y 119) (direction down) (internal ahead) (old-x 57) (old-y 118)) !

(tick 457)
collision pass : ((train-no 16) (x 57) (y 119) (direction down) (internal ahead) (old-x 57) (old-y 118)) ((train-no 2) (x 57) (y 118) (direction up) (internal ahead) (old-x 57) (old-y 119)) !
trains remaining 13 

(tick 471)
collision conv : ((train-no 9) (x 131) (y 74) (direction down) (internal right) (old-x 131) (old-y 73)) ((train-no 3) (x 131) (y 74) (direction right) (internal right) (old-x 130) (old-y 74)) ! 

(tick 471)
collision conv : ((train-no 3) (x 131) (y 74) (direction right) (internal right) (old-x 130) (old-y 74)) ((train-no 9) (x 131) (y 74) (direction down) (internal right) (old-x 131) (old-y 73)) ! 
trains remaining 11 

(tick 492)
collision pass : ((train-no 17) (x 62) (y 36) (direction right) (internal ahead) (old-x 61) (old-y 36)) ((train-no 5) (x 61) (y 36) (direction left) (internal left) (old-x 62) (old-y 36)) !

(tick 492)
collision pass : ((train-no 5) (x 61) (y 36) (direction left) (internal left) (old-x 62) (old-y 36)) ((train-no 17) (x 62) (y 36) (direction right) (internal ahead) (old-x 61) (old-y 36)) !
trains remaining 9 

(tick 1023)
collision conv : ((train-no 10) (x 54) (y 40) (direction left) (internal right) (old-x 55) (old-y 40)) ((train-no 14) (x 54) (y 40) (direction up) (internal ahead) (old-x 54) (old-y 41)) ! 

(tick 1023)
collision conv : ((train-no 14) (x 54) (y 40) (direction up) (internal ahead) (old-x 54) (old-y 41)) ((train-no 10) (x 54) (y 40) (direction left) (internal right) (old-x 55) (old-y 40)) ! 
trains remaining 7 

(tick 1324)
collision pass : ((train-no 6) (x 138) (y 115) (direction down) (internal right) (old-x 138) (old-y 114)) ((train-no 12) (x 138) (y 114) (direction up) (internal left) (old-x 138) (old-y 115)) !
(tick 1324)
collision pass : ((train-no 12) (x 138) (y 114) (direction up) (internal left) (old-x 138) (old-y 115)) ((train-no 6) (x 138) (y 115) (direction down) (internal right) (old-x 138) (old-y 114)) !
trains remaining 5 


(tick 2464)
collision pass : ((train-no 7) (x 36) (y 81) (direction left) (internal right) (old-x 37) (old-y 81)) ((train-no 13) (x 37) (y 81) (direction right) (internal left) (old-x 36) (old-y 81)) !
(tick 2464)
collision pass : ((train-no 13) (x 37) (y 81) (direction right) (internal left) (old-x 36) (old-y 81)) ((train-no 7) (x 36) (y 81) (direction left) (internal right) (old-x 37) (old-y 81)) !

trains remaining 3 

(tick 32942)
collision conv : ((train-no 1) (x 40) (y 17) (direction down) (internal left) (old-x 40) (old-y 16)) ((train-no 4) (x 40) (y 17) (direction up) (internal left) (old-x 40) (old-y 18)) ! 
(tick 32942)
collision conv : ((train-no 4) (x 40) (y 17) (direction up) (internal left) (old-x 40) (old-y 18)) ((train-no 1) (x 40) (y 17) (direction down) (internal left) (old-x 40) (old-y 16)) ! 

trains remaining 1 
((train-no 15) (x 131) (y 123) (direction right) (internal right) (old-x 130) (old-y 123))


|#











#|

may go one step more ...

116,99

|#
