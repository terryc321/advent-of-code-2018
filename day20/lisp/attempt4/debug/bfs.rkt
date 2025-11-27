#lang racket

;; bfs - breadth first search
;; grid quite some size
;; reuse it to store step count ... also have to check we can pass through door in
;; correct direction or else we violate game


(define-syntax format2
  (syntax-rules ()
    ((_ port body ...) (cond
                         ((equal? port #t) (fprintf (current-output-port) (format body ...)))
                         (#t (fprintf port (format body ...)))))))

(define-syntax dmesg
  (syntax-rules ()
    ((_ port body ...) (when #f ;; enable debug message change this one #f to #t 
			 (cond
                          ((equal? port #t) (fprintf (current-output-port) (format body ...)))
                          (#t (fprintf port (format body ...))))))))




;;(define board #f)
;; stklos
;;(require "srfi-1")
;;(define board #f)
;;(load "../results/results.scm")

(define *board-data* (second (third (call-with-input-file "../results/results.scm"
				    (lambda (port)
                                      (read port))))))
(format2 #t "board has been loaded!~%")


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

;; (define *arbitrary-large-integer* (* 2 2400 2400))
(define *board* (make-2d-array 2400 2400 #f))
;; (define *score* (make-2d-array 2400 2400 #f))

;; array-ref2  *board* x y    : x,y 1 based index to 2400 inclusive
;; array-set2! *board* x y n  : x,y 1 based index to 2400 inclusive

;; if we put walls everywhere - then knock them out if we have something else to
;; put it its place
(define (verify-board)
  (format2 #t "*step-array* read and write test ... ")
  (do ((y 1 (+ y 1)))
      ((>= y 2400) #f)
    (do ((x 1 (+ x 1)))
	((>= x 2400) #f)
      (let ((out (array-ref2 *board* x y)))
	(array-set2! *board* x y #\#)
	;; (array-set2! *score* x y #f)
	)))
  ;; we passed if we reach this far
  (format2 #t "passed ~%"))


;; real coordinate (0,0) ...... becomes (1200,1200) in ARRAY encoding
;;                        <<<..... decoding we need subtract 1200,1200
(define (translate-in-x x)  (+ x 1200))
(define (translate-in-y y)  (+ y 1200))
(define (translate-out-x x)  (- x 1200))
(define (translate-out-y y)  (- y 1200))

;; (define (safe-score-ref bd x y)
;;   (let ((tx (translate-in-x x))
;; 	(ty (translate-in-y y)))    
;;   (cond
;;    ((onboard? tx ty) (array-ref2 bd tx ty))
;;    (#t #f))))

(define (safe-board-set! bd x y n)
  (let ((tx (translate-in-x x))
	(ty (translate-in-y y)))    
  (cond
   ((onboard? tx ty) (array-set2! bd tx ty n))
   (#t #f))))


(define (read-board bd x y)
  (let ((tx (translate-in-x x))
	(ty (translate-in-y y)))    
  (cond
   ((onboard? tx ty) (array-ref2 bd tx ty))
   (#t #f))))

(define (mark-board! bd x y data)
  (let* ((tx (translate-in-x x))
	 (ty (translate-in-y y)))
    (cond
     ((onboard? tx ty) (array-set2! bd tx ty data))
     (#t (error "mark-board! not on board !")))))



(define (show-board)
  (call-with-output-file "../visuals/oval"
    (lambda (port)
      (format2 port "~%~%")
      (do ((y 1 (+ y 1)))
	  ((>= y 2400) #f)
	(format2 port "~%")
    (do ((x 1 (+ x 1)))
	((>= x 2400) #f)
      (let* ((val (array-ref2 *board* x y)))
	(cond
	 ((not val) (format2 port "~a" #\# ))
	 (port (format2 port "~a" val))))))
      (format2 port "~%~%"))))



;; load relevant file
;; load the array 2d 
;; loop through them and marking *board* as we go along
;; anywhere not marked is already a wall #\#
;;
;;  [ do this to start us off ]
;;           
;; > (run) 
;;


;; read *board-data*
(define (run)  
  (letrec ((foo (lambda (xs)
		  (cond
		    ((null? xs) #f)
		    (#t (let* ((triple (car xs))
			       (x (first triple))
			       (y (second triple))
			       (ch (third triple)))
			  (mark-board! *board* x y ch)
			  (foo (cdr xs))))))))    
    (foo *board-data*))
  ;; label start square as step zero !
  (mark-board! *board* 0 0 0))

  ;; (show-board))


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



;; bfs-help
;; - working-stack
;; - accumulator-stack
;; - current-step 
(define (bfs-help working accumulator step)
  (cond
   ((null? working)
    (cond    ;; empty todo or next generation  
     ((null? accumulator) 'done)
     (#t (bfs-transition accumulator step))))
   (#t (let* ((coord (car working))
	      (x (car coord))
	      (y (car (cdr coord))))
	 ;; can i go NORTH ? if door is trapdoor #\_ and end square empty
	 (let ((ch1 (read-board *board* x (+ y 0)))
	       (ch2 (read-board *board* x (+ y 1)))
	       (ch3 (read-board *board* x (+ y 2))))	   
	   (when (and (equal? ch2 #\_)
		      (equal? ch3 #\.))
	     ;; mark then add
	     (mark-board! *board* x (+ y 2) step)
	     (set! accumulator (cons (list x (+ y 2)) accumulator))))
	 ;; can i go SOUTH ? if door is trapdoor #\_ and end square empty
	 (let ((ch1 (read-board *board* x (+ y 0)))
	       (ch2 (read-board *board* x (+ y -1)))
	       (ch3 (read-board *board* x (+ y -2))))	   
	   (when (and (equal? ch2 #\_)
		      (equal? ch3 #\.))
	     ;; mark then add
	     (mark-board! *board* x (+ y -2) step)
	     (set! accumulator (cons (list x (+ y -2)) accumulator))))
	 ;; can i go EAST ? if door is trapdoor #\| and end square empty
	 (let ((ch1 (read-board *board* (+ x 0) y))
	       (ch2 (read-board *board* (+ x 1) y))
	       (ch3 (read-board *board* (+ x 2) y)))
	   (when (and (equal? ch2 #\|)
		      (equal? ch3 #\.))
	     ;; mark then add
	     (mark-board! *board* (+ x 2) y step)
	     (set! accumulator (cons (list (+ x 2) y) accumulator))))
	 ;; can i go WEST ? if door is trapdoor #\| and end square empty
	 (let ((ch1 (read-board *board* (+ x 0) y))
	       (ch2 (read-board *board* (+ x -1) y))
	       (ch3 (read-board *board* (+ x -2) y)))
	   (when (and (equal? ch2 #\|)
		      (equal? ch3 #\.))
	     ;; mark then add
	     (mark-board! *board* (+ x -2) y step)
	     (set! accumulator (cons (list (+ x -2) y) accumulator))))
	 
	 ;; next candidate
	 (bfs-help (cdr working) accumulator step)))))




;; using intermediate procedure so we can report on progress to user
(define (bfs-transition working step)
  (let ((next (+ step 1)))
    (format2 #t "bfs line 232 : transition to step ~a : size working list ~a ~%" next (length working))
    (bfs-help working '() next)))


(define (bfs)
  (let ((x 0)(y 0)(step 0))
    (mark-board! *board* x y step)
    (format2 #t "bfs line 239 ~%")
    (bfs-help (list (list x y))
	      '()
	      (+ step 1))))


(define (find-highest)
  (let ((highest 0)(highest-pt #f))
    (do ((y 1 (+ y 1)))
	((>= y 2400) #f)
      (do ((x 1 (+ x 1)))
	  ((>= x 2400) #f)
	(let* ((val (array-ref2 *board* x y)))
	  (cond
	   ((and (integer? val) (> val highest))
	    (set! highest val)
	    (set! highest-pt (list x y '=> (translate-out-x x) (translate-out-y y))))))))
    (format2 #t "the highest was ( ~a ) and found at location ( ~a ) ~%"
	    highest
	    highest-pt)
    (values highest highest-pt)))


    
(define (go)    
  (run)
  (mark-board! *board* 0 0 0) ;; was an #\X
  (format2 #t "~%starting BFS ...~%")
  (bfs)
  (format2 #t "done.~%")
  (format2 #t "finding highest step possible ...~%")
  (find-highest))


  ;; (format2 #t "~%finding highest location and steps ...~%")
  ;; (call-with-values
  ;;     (lambda () (find-highest))
  ;;   (lambda (highest highest-pt)    
  ;;     (format2 #t "done.~%")
  ;;     (format2 #t "highest was ~a ... at ~a ~%" highest highest-pt))))



;;(go)

