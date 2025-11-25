

(import (chicken format)) ;; format 
(import (chicken pretty-print)) 
(import (chicken condition)) ;; error handling

(import srfi-1)
(import srfi-69)
(import srfi-179) ;;
;; (import srfi-231) ;; improved 179
(import bindings) ;; destructuring

(define *endpoints*
  (call-with-input-file "output/end-points.scm"
    (lambda (port)
      (second (third (read port))))))

(define *open-doors*
  (call-with-input-file "output/open-doors.scm"
    (lambda (port)
      (second (third (read port))))))


(define min-max
  (let ((min-x 0)(max-x 0)(min-y 0)(max-y 0))
    (lambda (xs)
      (cond
       ((null? xs) `((min-x ,min-x)
		     (max-x ,max-x)
		     (min-y ,min-y)
		     (max-y ,max-y)))
       (#t (let* ((pt (car xs))
		  (x (first pt))
		  (y (second pt)))
	     (when (< x min-x) (set! min-x x))
	     (when (> x max-x) (set! max-x x))
	     (when (< y min-y) (set! min-y y))
	     (when (> y max-y) (set! max-y y))
	     (min-max (cdr xs))))))))


(define *board-endpoints* (min-max *endpoints*))

(define *board-doors* (min-max *open-doors*))


;; electric-indent-mode disable
;; rectangle-mark-mode
;;
;;        (0,2)
;;          |
;;        (0,1)* 
;;          |
;; (-1,0)-(0,0) -> *(1,0) -> (2,0)
;;          |
;;        (0,-1)
;;          |
;;        (0,-2)
;;
;; if start at 0,0 how can an open door be on anything other than odd square ?
;; because moving is a two-step process
;; - correction - starts on even number ,move right X=odd Y=even 
;;   correction - atleast one X or Y will be odd , maybe even both
(define odd-open-door-hypothesis
  (lambda (xs)
    (cond
     ((null? xs) #t)
     (#t (let* ((pt (car xs))
		(x (first pt))
		(y (second pt)))
	   (cond
	    ((or (odd? x) (odd? y))
	     (odd-open-door-hypothesis (cdr xs)))
	    (#t
	     (values #f pt))))))))

;;
;;(odd-open-door-hypothesis *open-doors*)
;;#t
;;
;;
;; > *board-endpoints*
;; (min-x=> -1166 max-x=> 1080 min-y=> -1030 max-y=> 1170)
;; > *board-doors*
;; (min-x=> -1166 max-x=> 1080 min-y=> -1032 max-y=> 1170)
;;
;; round down/up to nearest even square
;;
;; X from -1166 to 1082
;; Y from -1032 to 1172


;; (import srfi-179) ;;
;;  (define a (make-array (make-interval '#(1 1) '#(11 11))
;;                         (lambda (i j)
;;                           (if (= i j)
;;                               (list i j 'same)
;;                               (list i j 'differ)))))

;; (define a_ (array-getter a))
;; (define a! (array-setter a))



(define (make-2d-array cols rows  init)
  (let ((v (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((= i rows) v)
      (vector-set! v i (make-vector cols init)))))

(define (array-ref2 a i j)
  (vector-ref (vector-ref a (- j 1)) (- i 1)))

(define (array-set2! a i j val)
  (vector-set! (vector-ref a (- j 1)) (- i 1) val))

(define a (make-2d-array 3 4 #f))

(array-set2! a 1 1 '(1 1))

a

(array-set2! a 3 4 '(3 4))

a

(array-set2! a 2 3 '(2 3))

a


(array-set2! a 2 1 '(2 1))

a

(array-set2! a 3 1 '(3 1))

a

(array-set2! a 3 2 '(3 2))

a



;; ;; Interval from (1,1) to (10,10)
;; (define iv (make-interval '#(1 1) '#(11 11)))

;; ;; Make a mutable array using a thunk:
;; (define a
;;   (make-array iv
;;     (lambda ()
;;       '(default))))   ;; initial contents

;; (define a_ (array-getter a))
;; (define a! (array-setter a))

;; we go one before min value so we can see / find a hard border on show-board
(define *board-min-x*
  (+ -1
     (min (second (assoc 'min-x *board-endpoints*))
	  (second (assoc 'min-x *board-doors*)))))

(define *board-min-y*
  (+ -1 
     (min (second (assoc 'min-y *board-endpoints*))
	  (second (assoc 'min-y *board-doors*)))))
  

(define *board-width* (+ 2 (max (abs (- (second (assoc 'min-x *board-endpoints*))
					(second (assoc 'max-x *board-endpoints*))))
				(abs (- (second (assoc 'min-x *board-doors*))
					(second (assoc 'max-x *board-doors*)))))))
				

(define *board-height* (+ 2 (max (abs (- (second (assoc 'max-y *board-endpoints*))
					 (second (assoc 'min-y *board-endpoints*))))
				 (abs (- (second (assoc 'max-y *board-doors*))
					 (second (assoc 'min-y *board-doors*)))))))
			  			  
(define *board-max-x* (+ -1 *board-min-x* *board-width*))
(define *board-max-y* (+ -1 *board-min-y* *board-height*))






;; default to wall
(define *board* (make-2d-array *board-width* *board-height* #\#))
;; some dummy large value
(define *board2* (make-2d-array *board-width* *board-height* #f))



;; translate any position (x,y) into zero based board coordinate (0,0) to (*board-width* - 1,*board-height* - 1)
(define (translate-x x)  (+ 1 (+ x (abs *board-min-x*))))

(define (translate-y y)  (+ 1 (+ y (abs *board-min-y*))))

;; (* *board-width* *board-height*)
;; 4967912
;; ~ approx 5 million squares 

;; 0 0 start square is empty . - ie not a wall
;; walls # ... represented by #\#
;; empty . ... represented by #\.
;; doors _ or | .... represented by #\_
;;
;; populate *board* with open-doors
;; populate *board* with end-points
;;
;; do we have visited points also ??

(define (check-array-endpoint thunk)
  (condition-case (thunk)
    [(exn) (print "endpoint exception")]))

(define (check-array-open-doors thunk)
  (condition-case (thunk)
    [(exn) (print "open door exception")]))



(define (populate-end-points)
  (format #t "end points ... populating ...~%")      
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) #f)
		   (#t (let* ((pt (car xs))
			      (x1 (first pt))
			      (y1 (second pt))
			      (x2 (translate-x x1))
			      (y2 (translate-y y1)))
			 ;; (handle-exceptions exn (begin (format #t "pop end point ~a ~a failed : ~a ~a : ~a ~%" x1 y1 x2 y2 pt))
			 ;;(format #t "populating end point : x2 ~a : y2 ~a <= x1 ~a : y1 ~a ~%" x2 y2 x1 y1)
			 (array-set2! *board* x2 y2 #\E)
			 (foo (cdr xs))))))))
    (foo *endpoints*)
    (format #t "end points ... populated .~%").    
    ))





(define (populate-open-doors)
  (format #t "open doors ... populating ...~%")      
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) #f)
		   (#t (let* ((pt (car xs))
			      (x1 (first pt))
			      (y1 (second pt))
			      (x2 (translate-x x1))
			      (y2 (translate-y y1)))
			 ;; (handle-exceptions exn (begin (format #t "pop open doors ~a ~a failed ~%" x y))
			 ;;(format #t "populating open door point : x2 ~a : y2 ~a <= x1 ~a : y1 ~a ~%" x2 y2 x1 y1)
			 (array-set2! *board* x2 y2 #\_)
			 (foo (cdr xs))))))))
    (foo *open-doors*)
    (format #t "open doors ... populated .~%").
    ))


(populate-end-points)
(populate-open-doors)

*board*

(define (sanity-check)
  (let ((bd (make-2d-array *board-width* *board-height* #\#)))
    (array-ref2 bd 2250 2204)))


(define (sanity-check2)
  (let ((bd (make-2d-array 4  5  #\#)))
    (array-ref2 bd 3 4)))


(define (show-all-board bd)
  (format #t "~%~%")
  (do ((y *board-min-y* (+ y 1)))
      ((>= y (+ *board-min-y* *board-height*)) #f)
    (format #t "~%")
    (do ((x *board-min-x* (+ x 1)))
	((>= x (+ *board-min-x* *board-width*)) #f)
      (let* ((tx (translate-x x))
	     (ty (translate-y y))
	     (e (array-ref2 bd tx ty)))
	(cond
	 ((and (= x 0) (= y 0)) (format #t "S"))
	 (#t (format #t "~a" e))))))
  (format #t "~%~%"))


(define (show-board bd from-x wid from-y hgt)
  (format #t "~%~%")
  (do ((y from-y (+ y 1)))
      ((>= y (+ from-y hgt)) #f)
    (format #t "~%")
    (do ((x from-x (+ x 1)))
	((>= x (+ from-x wid)) #f)
      (let* ((tx (translate-x x))
	     (ty (translate-y y))
	     (e (array-ref2 bd tx ty)))
	(cond
	 ((and (= x 0) (= y 0)) (format #t "S"))
	 (#t (format #t "~a" e))))))
  (format #t "~%~%"))


(define (padding s n)
  (let ((str (format #f "~a" s)))
    (let loop ()
      (let ((slen (string-length str)))
	(when (< slen n)
	  (set! str (string-append " " str))
	  (loop))))
    str))


;; dps integer places to show
(define (show-board-numerical bd from-x wid from-y hgt pad-n)
  (format #t "~%~%")
  (do ((y from-y (+ y 1)))
      ((>= y (+ from-y hgt)) #f)
    (format #t "~%")
    (do ((x from-x (+ x 1)))
	((>= x (+ from-x wid)) #f)
      (let* ((tx (translate-x x))
	     (ty (translate-y y))
	     (e (array-ref2 bd tx ty)))
	(cond
	 ((and (= x 0) (= y 0)) (format #t (padding "S" pad-n)))
	 ((not e) (format #t (padding "" pad-n)))
	 (#t (format #t "~a" (padding e pad-n)))))))
  (format #t "~%~%"))




;; we can show a sub-region -- here we note that (0,0) is marked as the start position
;; (show-board 0 5 0 5)
;;
;; S_E_E
;; _#_#_
;; E_E_E
;; _#_#_
;; E_E_E


(define (copy-all-board!)
  (do ((y (+ 1 *board-min-y*) (+ y 1)))
      ((>= y (+ *board-min-y* (- *board-height* 2))) #f)
    (do ((x (+ 1 *board-min-x*) (+ x 1)))
	((>= x (+ *board-min-x* (- *board-width* 2))) #f)
      (let* ((tx (translate-x x))
	     (ty (translate-y y))
	     (e (array-ref2 *board* tx ty)))
	(array-set2! *board2* tx ty e)))))


;; *board2* from *board*
;;(copy-all-board!)


(define (onboard? x y)
  (let* ((tx (translate-x x))
	 (ty (translate-y y)))
    (and (>= tx 1) (<= tx (+ *board-width*))
	 (>= ty 1) (<= ty (+ *board-height*)))))

;; note - call onboard? x y first !
(define (wall? x y)
  (let* ((tx (translate-x x))
	 (ty (translate-y y)))
    (let ((e (array-ref2 *board* tx ty)))
      (cond
       ((and (char? e) (char=? e #\E)) #f)
       ((and (char? e) (char=? e #\_)) (error "called wall? on a door? how is that possible?"))
       ((and (char? e) (char=? e #\#)) #t)
       (#t (error "wall? - not E _ or # ? how come? "))))))
  
(define (door? x y)
  (cond
   ((not (onboard? x y)) #f)
   (#t 
    (let* ((tx (translate-x x))
	   (ty (translate-y y)))
      (let ((e (array-ref2 *board* tx ty)))
	(cond
	 ((and (char? e) (char=? e #\_)) #t)
	 (#t #f)))))))
  



(define (bfs x y n)
  (cond
   ((not (onboard? x y)) #f) ;; cannot label off board positions
   ((wall? x y) #f) ;; cannot label walls
   (#t ;; onboard atleast
    (let* ((tx (translate-x x))
	   (ty (translate-y y))
	   (e (array-ref2 *board2* tx ty)))
      (cond
       ((not e) (array-set2! *board2* tx ty n)
	(when (door? (+ x 1) y) (bfs (+ x 2) y (+ n 1)))
	(when (door? (- x 1) y) (bfs (- x 2) y (+ n 1)))
	(when (door? x (+ y 1)) (bfs x (+ y 2) (+ n 1)))
	(when (door? x (- y 1)) (bfs x (- y 2) (+ n 1))))
       ((< e n) #t)
       ((= e n) #t)
       ((> e n) (array-set2! *board2* tx ty n)
	(when (door? (+ x 1) y) (bfs (+ x 2) y (+ n 1)))
	(when (door? (- x 1) y) (bfs (- x 2) y (+ n 1)))
	(when (door? x (+ y 1)) (bfs x (+ y 2) (+ n 1)))
	(when (door? x (- y 1)) (bfs x (- y 2) (+ n 1)))))))))

;; (run-bfs) ... hypothesis is that this algorithm will not complete in a reasonable amount of time
(define (run-bfs)
  (format #t "running bfs . ~%")
  (bfs 0 0 0)
  (format #t "completed bfs . ~%"))



;; donut emoji - is this unicode ??
(define 🍩 3)

;; scan from left side of map till we get first E ... this will be the furthest out
(define (lhs-board)
  (let ((max-dist 0)(dist-list '())(tox (translate-x 0))(toy (translate-y 0)))
    (do ((y (+ *board-min-y*) (+ y 1)))
	((> y (+ *board-min-y* (- *board-height* 1))) #f)
      (call/cc (lambda (continue)
		 (do ((x (+ *board-min-x*) (+ x 1)))
		     ((> x (+ *board-min-x* (- *board-width* 1))) #f)
		   (let* ((tx (translate-x x))
			  (ty (translate-y y))
			  (e (array-ref2 *board* tx ty)))
		     (cond
		      ((and (char? e) (char=? e #\E)) ;;found endpoint
		       (let* ((dist-x (abs (- tx tox)))
			      (dist-y (abs (- ty toy)))
			      (tot-dist (+ dist-x dist-y)))
			 (format #t "~a : ~a ~a : dist ~a ~a : tot dist ~a ~%" y tx ty dist-x dist-y tot-dist)
			 (cond
			  ((> tot-dist max-dist)
			   (set! max-dist tot-dist)
			   (set! dist-list (list (list x y))))
			  ((= tot-dist max-dist)
			   (set! dist-list (cons (list x y) dist-list))))			 
			 (continue #t)))))))))
    (format #t "LHS max-dist ~a ~%" max-dist)
    (format #t "LHS dist-list ~a ~%" dist-list)
    dist-list))
;;(values max-dist dist-list)))


;; scan from right side of map till we get first E ... this will be the furthest out from left
(define (rhs-board)
  (let ((max-dist 0)(dist-list '())(tox (translate-x 0))(toy (translate-y 0)))
    (do ((y (+ *board-min-y*) (+ y 1)))
	((> y *board-max-y*) #f) ;; stop condition
      (call/cc (lambda (continue)
		 (do ((x (+ *board-max-x*) (- x 1)))
		     ((< x *board-min-x*) #f)
		   (let* ((tx (translate-x x))
			  (ty (translate-y y))
			  (e (array-ref2 *board* tx ty)))
		     (cond
		      ((and (char? e) (char=? e #\E)) ;;found endpoint
		       (let* ((dist-x (abs (- tx tox)))
			      (dist-y (abs (- ty toy)))
			      (tot-dist (+ dist-x dist-y)))
			 (format #t "~a : ~a ~a : dist ~a ~a : tot dist ~a ~%" y tx ty dist-x dist-y tot-dist)
			 (cond
			  ((> tot-dist max-dist)
			   (set! max-dist tot-dist)
			   (set! dist-list (list (list x y))))
			  ((= tot-dist max-dist)
			   (set! dist-list (cons (list x y) dist-list))))			 
			 (continue #t)))))))))
    (format #t "RHS max-dist ~a ~%" max-dist)
    (format #t "RHS dist-list ~a ~%" dist-list)
    (values max-dist dist-list)))


;; LHS max-dist 1706 
;; RHS max-dist 1618 

;;
;; > (translate-x 0)
;; 1168
;; > (translate-y 0)
;; 1034
;;

;; so if we assume we can go any direction (/ 1706 2)
;; 853

;; 1706 ??
;; still too low












                                                                                  
  



