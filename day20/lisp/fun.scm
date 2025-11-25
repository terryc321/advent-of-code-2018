

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
  (vector-ref (vector-ref a i) j))

(define (array-set2! a i j val)
  (vector-set! (vector-ref a i) j val))

;; ;; Interval from (1,1) to (10,10)
;; (define iv (make-interval '#(1 1) '#(11 11)))

;; ;; Make a mutable array using a thunk:
;; (define a
;;   (make-array iv
;;     (lambda ()
;;       '(default))))   ;; initial contents

;; (define a_ (array-getter a))
;; (define a! (array-setter a))

(define *board-min-x* (second (assoc 'min-x *board-endpoints*)))

(define *board-min-y* (second (assoc 'min-y *board-endpoints*)))
  

(define *board-width* (+ 6 (abs (- (second (assoc 'min-x *board-endpoints*))
				   (second (assoc 'max-x *board-endpoints*))))))

(define *board-height* (+ 6 (abs (- (second (assoc 'max-y *board-endpoints*))
				    (second (assoc 'min-y *board-endpoints*))))))


;; default to wall
(define *board* (make-2d-array *board-width* *board-height* #\#))

;; translate any position (x,y) into zero based board coordinate (0,0) to (*board-width* - 1,*board-height* - 1)
(define (translate-x x)  (+ x (abs *board-min-x*)))

(define (translate-y y)  (+ y (abs *board-min-y*)))


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
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) #f)
		   (#t (let* ((pt (car xs))
			      (x1 (first pt))
			      (y1 (second pt))
			      (x (translate-x x1))
			      (y (translate-y y1)))
			 (handle-exceptions exn (begin (format #t "pop end point ~a ~a failed : ~a ~a : ~a ~%" x1 y1 x y pt))
			   (array-set2! *board* x y #\E)
			   (foo (cdr xs)))))))))
    (foo *endpoints*)))



(define (populate-open-doors)
  (letrec ((foo (lambda (xs)
		  (cond
		   ((null? xs) #f)
		   (#t (let* ((pt (car xs))
			      (x (translate-x (first pt)))
			      (y (translate-y (second pt))))
			 (handle-exceptions exn (begin (format #t "pop open doors ~a ~a failed ~%" x y))
			   (array-set2! *board* x y #\_)
			   (foo (cdr xs)))))))))
    (foo *open-doors*)))


(populate-end-points)
(populate-open-doors)

*board*






                                                                                  
  



