;; #lang racket


;; stklos
(require "srfi-1")


;; two dimenstional array
(define (make-2d-array cols rows  init)
  (let ((v (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((= i rows) v)
      (vector-set! v i (make-vector cols init)))))

(define (array-ref2 a i j)
  (vector-ref (vector-ref a (- j 1)) (- i 1)))

(define (array-set2! a i j val)
  (vector-set! (vector-ref a (- j 1)) (- i 1) val))

(define *arbitrary-large-integer* (* 2 2400 2400))
(define *board* (make-2d-array 2400 2400 #f))
(define *score* (make-2d-array 2400 2400 #f))

;; array-ref2  *board* x y    : x,y 1 based index to 2400 inclusive
;; array-set2! *board* x y n  : x,y 1 based index to 2400 inclusive

;; if we put walls everywhere - then knock them out if we have something else to
;; put it its place
(define (verify-board)
  (format #t "*step-array* read and write test ... ")
  (do ((y 1 (+ y 1)))
      ((>= y 2400) #f)
    (do ((x 1 (+ x 1)))
	((>= x 2400) #f)
      (let ((out (array-ref2 *board* x y)))
	(array-set2! *board* x y #\#)
	(array-set2! *score* x y #f)
	)))
  ;; we passed if we reach this far
  (format #t "passed ~%"))


;; real coordinate (0,0) ...... becomes (1200,1200) in ARRAY encoding
;;                        <<<..... decoding we need subtract 1200,1200
(define (translate-in-x x)  (+ x 1200))
(define (translate-in-y y)  (+ y 1200))
(define (translate-out-x x)  (- x 1200))
(define (translate-out-y y)  (- y 1200))

;; mark-board! simply writes data to grid square x y
(define (mark-board! bd x y data)
  (let* ((tx (translate-in-x x))
	 (ty (translate-in-y y)))
    (array-set2! bd tx ty data)))


(define (show-board)
  (call-with-output-file "visuals/oval"
    (lambda (port)
      (format port "~%~%")
      (do ((y 1 (+ y 1)))
	  ((>= y 2400) #f)
	(format port "~%")
    (do ((x 1 (+ x 1)))
	((>= x 2400) #f)
      (let* ((val (array-ref2 *board* x y)))
	(cond
	 ((not val) (format port "~a" #\# ))
	 (port (format port "~a" val))))))
      (format port "~%~%"))))



;; load relevant file
;; load the array 2d 
;; loop through them and marking *board* as we go along
;; anywhere not marked is already a wall #\#
;;
;;  [ do this to start us off ]
;;           
;; > (run) 
;;
(define board #f)

(define (run)
  (load "results/results.scm")
  (letrec ((foo (lambda (xs)
		  (cond
		    ((null? xs) #f)
		    (#t (let* ((triple (car xs))
			       (x (first triple))
			       (y (second triple))
			       (ch (third triple)))
			  (mark-board! *board* x y ch)
			  (foo (cdr xs))))))))
    (foo board))
  ;; label start square
  (mark-board! *board* 0 0 #\X)
  (show-board))


;; need 2nd board *score*
;; North - increase Y by 2 , must be trapdoor #\_ at (X,Y+1)
;; Sorth - increase Y by -2 , must be trapdoor #\_ at (X,Y-1)
;; East - increase X by 2 , must be sidedoor #\| at (X+1,Y)
;; West - increase X by -2 , must be sidedoor #\| at (X-1,Y)
;;
;; to be able to go north , need trapdoor #\_ at (x , y +1) and 
;; square empty #\. at (x , y+2)
;;

(define (onboard? x y)
  (and (>= x 1) (<= x 2400)
       (>= y 1) (<= y 2400)))

(define (safe-score-ref bd x y)
  (let ((tx (translate-in-x x))
	(ty (translate-in-y y)))    
  (cond
   ((onboard? tx ty) (array-ref2 bd tx ty))
   (#t #f))))

(define (safe-score-set! bd x y n)
  (let ((tx (translate-in-x x))
	(ty (translate-in-y y)))    
  (cond
   ((onboard? tx ty) (array-set2! bd tx ty n))
   (#t #f))))


(define (safe-board-ref bd x y)
  (let ((tx (translate-in-x x))
	(ty (translate-in-y y)))    
  ;; (format #t "safe-array-ref2 BD ~a ~a~%" tx ty)
  (cond
   ((onboard? tx ty) (array-ref2 bd tx ty))
   (#t #f))))
	    


(define (bfs x y n)
  "breadth first search"
  ;; NORTH
  (format #t "bfs : ~a ~a ~a : at north." x y n)
  (let ((ch1 (safe-board-ref *board* x (+ y 0)))
	(ch2 (safe-board-ref *board* x (+ y 1)))
	(ch3 (safe-board-ref *board* x (+ y 2))))
    ;; whenever we land on a square we try to claim it
    (let ((val (safe-score-ref *score* x y)))      
      (when (or (not val) (< n val))
	(safe-score-set! *score* x y n)))    
    (when (and ch1 ch2 ch3
	       (char=? ch3 #\.)
	       (char=? ch2 #\_)
	       (char=? ch1 #\.))      
      (bfs x (+ y 2) (+ n 1))))
  ;; SOUTH
  (format #t "bfs : ~a ~a ~a : at south." x y n)
  (let ((ch1 (safe-board-ref *board* x (+ y 0)))
	(ch2 (safe-board-ref *board* x (+ y -1)))
	(ch3 (safe-board-ref *board* x (+ y -2))))
    (when (and ch1 ch2 ch3
	       (char=? ch1 #\.)
	       (char=? ch2 #\_)
	       (char=? ch3 #\.))
      (bfs x (+ y -2) (+ n 1))))
  ;;EAST
  (format #t "bfs : ~a ~a ~a : at east." x y n)
  (let ((ch1 (safe-board-ref *board* (+ x 0) y))
	(ch2 (safe-board-ref *board* (+ x 1) y))
	(ch3 (safe-board-ref *board* (+ x 2) y)))
    (when (and ch1 ch2 ch3
	       (char=? ch1 #\.)(char=? ch2 #\|)(char=? ch3 #\.))
      (bfs (+ x 2) y (+ n 1))))
  
  ;; WEST
  (format #t "bfs : ~a ~a ~a : at west." x y n)
  (let ((ch1 (safe-board-ref *board* (+ x 0) y))
	(ch2 (safe-board-ref *board* (+ x -1) y))
	(ch3 (safe-board-ref *board* (+ x -2) y)))
    (when (and ch1 ch2 ch3
	       (char=? ch3 #\.)(char=? ch2 #\|)(char=? ch1 #\.))
      (bfs (+ x -2) y (+ n 1)))))



(define (find-highest)
  (let ((highest 0)(highest-pt #f))
    (do ((y 1 (+ y 1)))
	((>= y 2400) #f)
      (do ((x 1 (+ x 1)))
	  ((>= x 2400) #f)
	(let* ((val (array-ref2 *score* x y)))
	  (cond
	   ((not val) #f) ;; a wall
	   ((> val highest)
	    (set! highest val)
	    (set! highest-pt (list x y '=> (translate-out-x x) (translate-out-y y))))
	   (#t #f)))))
    (format #t "the highest was ( ~a ) and found at location ( ~a ) ~%"
	    highest
	    highest-pt)
    (values highest highest-pt)))



    
(define (go)    
  (run)
  (mark-board! *board* 0 0 #\.) ;; was an #\X
  (format #t "~%~%starting BFS ...~%")
  (bfs 0 0 0)
  (format #t "done.~%")
  (format #t "~%finding highest location and steps ...~%")
  (call-with-values
      (lambda () (find-highest))
    (lambda (highest highest-pt)    
      (format #t "done.~%")
      (format #t "highest was ~a ... at ~a ~%" highest highest-pt))))



(go)



