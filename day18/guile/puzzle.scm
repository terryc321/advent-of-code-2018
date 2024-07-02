
(import (ice-9 format))
(import (ice-9 pretty-print))
(import (srfi srfi-1))

;;----------------------------------------------------------------------------------
;; rewriting a 2d grid vector for 500 th time again
;; why is there no library for this in scheme ?
;; no loop iterations in guile
(define make-grid
  (lambda (wid hgt)
    (let ((top (make-vector (+ hgt 2) 0)))
      (letrec ((foo (lambda (i)
		      (cond
		       ((< i 1) #t)
		       (#t (let ((vec (make-vector (+ wid 2) 0)))
			     (vector-set! top i vec)
			     (foo (- i 1))))))))
	(foo hgt)
	`((type grid) (width ,wid) (height ,hgt) (data ,top))))))

(define grid-width
  (lambda (grid)  (second (assoc 'width grid))))

(define grid-height
  (lambda (grid)  (second (assoc 'height grid))))

(define grid-data
  (lambda (grid)  (second (assoc 'data grid))))

(define grid-onboard?
  (lambda (grid x y)
    (let ((wid (grid-width grid))
	  (hgt (grid-height grid)))
      (and (>= x 1) (<= x wid)
	   (>= y 1) (<= y hgt)))))

(define grid-xy
  (lambda (grid x y)
    (let ((wid (grid-width grid))
	  (hgt (grid-height grid))
	  (data (grid-data grid)))
      (cond
       ((grid-onboard? grid x y)
	(vector-ref (vector-ref data y) x))
       (#t (error (format #f "grid-xy offboard ~a ~a fpr ~a ~a" x y  wid hgt)))))))

(define grid-xy!
  (lambda (grid x y z)
    (let ((wid (grid-width grid))
	  (hgt (grid-height grid))
	  (data (grid-data grid)))
      (cond
       ((grid-onboard? grid x y)
	(vector-set! (vector-ref data y) x z))
       (#t (error (format #f "grid-xy! offboard ~a ~a fpr ~a ~a" x y  wid hgt)))))))


(define show-grid
  (lambda (grid)
    (let ((wid (grid-width grid))
	  (hgt (grid-height grid)))
      (format #t "~%")
      (letrec ((foo (lambda (x y)
		      (cond
		       ((> x wid)
			(format #t "~%")
			(foo 1 (+ y 1)))
		       ((> y hgt) #f)
		       (#t (let ((val (grid-xy grid x y)))
			     (format #t "~a " val)
			     (foo (+ x 1) y)))))))
	(foo 1 1)
	#f))))


;; create a duplicate of grid 
(define copy-grid
  (lambda (grid)
    (let* ((wid (grid-width grid))
	   (hgt (grid-height grid))
	   (res (make-grid wid hgt)))	  
      (letrec ((foo (lambda (x y)
		      (cond
		       ((> x wid)
			(foo 1 (+ y 1)))
		       ((> y hgt) #f)
		       (#t (let ((val (grid-xy grid x y)))
			     (grid-xy! res x y val)
			     (foo (+ x 1) y)))))))
	(foo 1 1)
	res))))

;;---------------------------------------------------------------------------------------


;; some i/o load the demo board into a grid
(define input-grid
  (lambda ()
    (let* ((wid 50)
	   (hgt 50)
	   (g (make-grid wid hgt)))
      (call-with-input-file "input"
	(lambda (port)
	  (letrec ((next-char (lambda ()
			   (let ((ch (read-char port)))
			     (cond
			      ((char=? ch #\#) '@)
			      ((char=? ch #\.) '_)
			      ((char=? ch #\|) ':)
			      (#t (next-char))))))
		   (foo (lambda (x y)
			  (cond
			   ((> x wid)
			    (foo 1 (+ y 1)))
			   ((> y hgt) #f)
			   (#t (let ((val (next-char)))
				 (grid-xy! g x y val)
				 (foo (+ x 1) y)))))))
	    (foo 1 1)
	    g))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; divert from puzzle notation 
;;            lisp    puzzle
;;  ---------------------------
;; lumberyard   @      #
;; open space   _      .
;; trees        :      |
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define has-tree
  (lambda (grid x y)
    (cond
     ((and (grid-onboard? grid x y)
	   (eq? (grid-xy grid x y) ':))
      1)
     (#t 0))))

(define has-lumber
  (lambda (grid x y)
    (cond
     ((and (grid-onboard? grid x y)
	   (eq? (grid-xy grid x y) '@))
      1)
     (#t 0))))




;; An open acre will become filled with trees if three or more
;; adjacent acres contained trees. Otherwise, nothing happens.
(define transition-open
  (lambda (grid x y res)
    (let ((n-tree (+ (has-tree grid (- x 1) (- y 1))
		     (has-tree grid (+ x 0) (- y 1))
		     (has-tree grid (+ x 1) (- y 1))
		     
		     (has-tree grid (- x 1) (- y 0))
		     (has-tree grid (+ x 1) (- y 0))
		     		     
		     (has-tree grid (- x 1) (+ y 1))
		     (has-tree grid (+ x 0) (+ y 1))
		     (has-tree grid (+ x 1) (+ y 1))
		     )))
      (cond
       ((>= n-tree 3) ;; become trees :
	(grid-xy! res x y ':))
       (#t ;; remain open acre _
	(grid-xy! res x y '_))))))


;; An acre filled with trees will become a lumberyard if three or more
;; adjacent acres were lumberyards. Otherwise, nothing happens.
(define transition-trees
  (lambda (grid x y res)
    (let ((n-lumber (+ (has-lumber grid (- x 1) (- y 1))
		       (has-lumber grid (+ x 0) (- y 1))
		       (has-lumber grid (+ x 1) (- y 1))
		       
		       (has-lumber grid (- x 1) (- y 0))
		       (has-lumber grid (+ x 1) (- y 0))
		       
		       (has-lumber grid (- x 1) (+ y 1))
		       (has-lumber grid (+ x 0) (+ y 1))
		       (has-lumber grid (+ x 1) (+ y 1))
		       )))
      (cond
       ((>= n-lumber 3) ;; become lumbers @
	(grid-xy! res x y '@))
       (#t ;; remain trees :
	(grid-xy! res x y ':))))))

;; An acre containing a lumberyard will remain a lumberyard if it was
;; adjacent to at least one other lumberyard and at least one acre
;; containing trees. Otherwise, it becomes open.
(define transition-lumber
  (lambda (grid x y res)
    (let ((n-tree (+ (has-tree grid (- x 1) (- y 1))
		     (has-tree grid (+ x 0) (- y 1))
		     (has-tree grid (+ x 1) (- y 1))
		     
		     (has-tree grid (- x 1) (- y 0))
		     (has-tree grid (+ x 1) (- y 0))
		     		     
		     (has-tree grid (- x 1) (+ y 1))
		     (has-tree grid (+ x 0) (+ y 1))
		     (has-tree grid (+ x 1) (+ y 1))
		     ))
	  (n-lumber (+ (has-lumber grid (- x 1) (- y 1))
		       (has-lumber grid (+ x 0) (- y 1))
		       (has-lumber grid (+ x 1) (- y 1))
		       
		       (has-lumber grid (- x 1) (- y 0))
		       (has-lumber grid (+ x 1) (- y 0))
		       
		       (has-lumber grid (- x 1) (+ y 1))
		       (has-lumber grid (+ x 0) (+ y 1))
		       (has-lumber grid (+ x 1) (+ y 1))
		       )))
      (cond
       ((and (>= n-lumber 1) (>= n-tree 1))  ;; stay lumber @ yard
	(grid-xy! res x y '@))
       (#t ;; otherwise become open acre _
	(grid-xy! res x y '_))))))



;; assume for sake of clarity width 10 , height 10 of demo board
(define transition
  (lambda (grid)
    (let ((wid (grid-width grid))
	  (hgt (grid-height grid))
	  (g (copy-grid grid))
	  )
      (letrec ((foo (lambda (x y)
		      (cond
		       ((> x wid)
			(foo 1 (+ y 1)))
		       ((> y hgt) #f)
		       (#t
			(let ((val (grid-xy grid x y)))
			  (cond
			   ;; grid and g in scope and mutable
			   ((eq? val '@ ) (transition-lumber grid x y g))
			   ((eq? val ': ) (transition-trees grid x y g))
			   ((eq? val '_ ) (transition-open grid x y g))))
			(foo (+ x 1) y))))))
	(foo 1 1)
	g))))


(define explore
  (lambda ()
    (let ((g (input-grid)))
      (letrec ((foo (lambda (ng i)
		      (format #t "~%~%")
		      (format #t "Transition ~a ~%" i)
		      (show-grid ng)
		      (format #t "~%")
		      (cond
		       ((>= i 10) (format #t "results => ~a ~%" (count ng)))
		       (#t
			(let ((ng2 (transition ng)))
			  (foo ng2 (+ i 1))))))))
	(foo g 0)))))



(define count
  (lambda (grid)
    (let ((wid (grid-width grid))
	  (hgt (grid-height grid))
	  (n-tree 0)
	  (n-open 0)
	  (n-lumber 0))
      (letrec ((foo (lambda (x y)
		      (cond
		       ((> x wid)
			(foo 1 (+ y 1)))
		       ((> y hgt) #f)
		       (#t
			(let ((val (grid-xy grid x y)))
			  (cond
			   ;; grid and g in scope and mutable
			   ((eq? val '@ ) (set! n-lumber (+ 1 n-lumber)))
			   ((eq? val ': ) (set! n-tree (+ 1 n-tree)))
			   ((eq? val '_ ) (set! n-open (+ 1 n-open))))
			(foo (+ x 1) y)))))))
	(foo 1 1)
	`(results (tree ,n-tree)
		  (open ,n-open)
		  (lumber ,n-lumber)
		  (tree*lumber ,(* n-tree n-lumber)))))))
    
			







	  




      



