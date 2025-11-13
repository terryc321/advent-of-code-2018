
(import (chicken format)) ;; format 
(import (chicken io)) ;; io read-line
(import (chicken process)) ;; ???
;; (import (chicken documentation))
(import (chicken process-context)) ;; current-directory / change-directory
(import (chicken pretty-print)) ;; pp 

(import regex) ;; string-match 

(import bindings) ;; bind

(import srfi-69) ;; hashes

;;(import procedural-macros) ;; define-macro
(import (chicken syntax)) ;; macro transformers

;; (current-directory)
;; (change-directory "day17/chicken")


(define get-input
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
	(let f ((x (read-line p)))
	  (if (eof-object? x)
              '()
	      (if (zero? (string-length x))
		  (f (read-line p))
		  (cons x (f (read-line p))))))))))


(define decipher 
  (let ((regex1 (regexp "x=([0-9]+).*y=([0-9]+)[.][.]([0-9]+)"))
	(regex2 (regexp "y=([0-9]+).*x=([0-9]+)[.][.]([0-9]+)")))
    (lambda (s)
      (let ((matches-a (string-match regex1 s))
	    (matches-b (string-match regex2 s)))
	(cond
	 (matches-a (cons 'x-y-y
			  (map (lambda (r) (string->number r 10))
			       (cdr matches-a))))
	 (matches-b (cons 'y-x-x
			  (map (lambda (r) (string->number r 10))
			       (cdr matches-b))))
	 (#t (error (format #f "no match on line ~a" s))))))))

(define range
  (lambda (expr)
    (cond
     ((eq? (car expr) 'x-y-y) (apply range-x-y-y (cdr expr)))
     ((eq? (car expr) 'y-x-x) (apply range-y-x-x (cdr expr)))
     (#t (error (format #f "range : bad x-y-y or y-x-x ~a in ~a" (car expr) expr))))))

(define range-x-y-y
  (lambda args
    (bind (x y1 y2) args ;;(format #t "making range x ~a : y ~a -> y ~a ~%" x y1 y2)
	  (recur-range-x-y-y x y1 y2))))

(define range-y-x-x
  (lambda args
    (bind (y x1 x2) args ;;(format #t "making range y ~a : x1 ~a -> x2 ~a ~%" y x1 x2)
	  (recur-range-y-x-x y x1 x2))))

(define recur-range-x-y-y
  (lambda (x y1 y2)
    (cond
     ((= y1 y2) (list (list x y1)))
     (#t (cons (list x y1) (recur-range-x-y-y x (+ y1 1) y2))))))

(define recur-range-y-x-x
  (lambda (y x1 x2)
    (cond
     ((= x1 x2) (list (list x1 y)))
     (#t (cons (list x1 y) (recur-range-y-x-x y (+ x1 1) x2))))))

(define fix
  (lambda (f)
    (apply append (map range (map decipher (get-input f))))))

(define input (fix "../input.txt"))
(define example (fix "../example.txt"))
(define puzzle (fix "../puzzle1.txt"))

;; =========== input example puzzle ==== appear to be a list of points =================
;; so far so good
;;
;; (find-limits input) => (404 628 1 1631)
;; (find-limits example) => (404 628 1 1631)
;;
;; =====================================================================================

(define clay-hash #f)

(define make-clay
  (lambda (points)
    (set! clay-hash (make-hash-table))
    (letrec ((foo (lambda (pt)
		    (hash-table-set! clay-hash pt #t))))
      (map foo points)		    
      (lambda (pt)
	(hash-table-ref/default clay-hash pt #f)))))

;; (define test-1
;;   (lambda ()
;;     (let ((f (clay input)))
;;       (map f input))))

(define find-limits
  (let ((min-x #f)(max-x #f)
	(min-y #f)(max-y #f))
    (lambda (points)
      (cond
       ((null? points) (list min-x max-x min-y max-y))
       (#t (let ((point (car points)))
	     (bind (x y) point
		   (when (or (not min-x) (< x min-x)) (set! min-x x))
		   (when (or (not max-x) (> x max-x)) (set! max-x x))
		   (when (or (not min-y) (< y min-y)) (set! min-y y))
		   (when (or (not max-y) (> y max-y)) (set! max-y y))
		   (find-limits (cdr points)))))))))


(define water-hash (make-hash-table))

(define water-count 0)

;; make an entry into water hash 
(define water!
  (lambda (pt)
    (set! water-count (+ 1 water-count))
    (hash-table-set! water-hash pt #t)))

(define water?
  (lambda (pt)
    (hash-table-ref/default water-hash pt #f)))

;; however write for loop in chicken scheme macros ??
;; (define-macro (for vargs . body)
;;   (bind (var start stop step) vargs
;; 	`(let ..)))



;; a.k.a defmacro 
(define-syntax @show-grid
  (er-macro-transformer
   (lambda (exp rename compare?)
     `(begin
	(newline)
	(letrec ((loop (lambda (x y)
			 (cond
			  ((> x max-x)
			   ;; newline
			   (newline)
			   (loop min-x (+ y 1)))
			  ((> y max-y) #f)
			  (#t
			   ;; do action here 
			   ;;(format #t "x = ~a y = ~a ~%" x y)
			   
			   ;; sprinkler head at 500 0 			       
			   (cond
			    ((and (= x 500) (= y 0)) (format #t "+"))
			    ((clay? (list x y)) (format #t "#"))
			    ((water? (list x y)) (format #t "~a" #\~))			    
			    (#t (format #t ".")))
			   ;; next in row 
			   (loop (+ x 1) y))))))
	  (loop min-x 0))))))


;; multiple hash tables - almost like layers of an image , can pick and choose which
;; layer goes next , in terms of priority
;; sprinkler at x = 500 , y = 0 . may not count sprinkler ...
;; 
;; can the water jet ever get less than y = 0 ?? possibly
;; see if we can determine that spill over is computed only on the basis of clay
;; not on the presence of water , avoids water spill over water spill over water
;; before know it whole diagram full of water
;;
;; if vert returns true , mean explicit about vert return value
;; if vert returns true we overflow upwards
;;
;; call k-done #t when we are blocked by clay going left or going right
;; call k-done #f when one or both sides are spill overs , which means we do not
;; overflow vertically upwards 
(define run
  (lambda (in)
    (call/cc (lambda (exit) 
    (let ((clay? (make-clay in)))
      (bind (min-x max-x min-y max-y) (find-limits in)
	    (format #t " ~a ~a ~a ~a ~%" min-x max-x min-y max-y)
	    ;; (@show-grid)
	    (define out? (lambda (x y) (or (< y 0)(> y max-y))))
	    (define vert (lambda (x y)
			   (cond
			    ((clay? (list x y)) (fill x (- y 1)))
			    ((water? (list x y)) #f) ;; already been here			    
			    ((out? x y) #f) ;; prevent infinite loop when outside pond
			    (#t
			     (water! (list x y))
			     (vert x (+ y 1))))))

	    (define fill (lambda (x y)
			   (water! (list x y))
			   (let ((L (call/cc (lambda (done) (fill-left x y done))))
				 (R (call/cc (lambda (done) (fill-right x y done)))))
			     ;;(format #t "L ~a : R ~a ~%" L R)
			     (cond ;; overflow vertically upwards
			      ((and L R) (fill x (- y 1))))
			     #t)))
	    
	    (define fill-left (lambda (x y done)
				(cond
				 ((clay? (list x y)) (done #t)) ;; hit clay
				 ((or (clay? (list x (+ y 1)))
				      (water? (list x (+ y 1))))
				  (water! (list x y))
				  (fill-left (- x 1) y done))
				 (#t
				  ;;spillover
				  ;; (water! (list x y))
				  ;; loop back to vert routine from this point
				  (vert x y)
				  ;; nothing more to do so save unwinding stack
				  ;; just cut it off at knees
				  (done #f)))))

	    (define fill-right (lambda (x y done)
				(cond
				 ((clay? (list x y)) (done #t)) ;; hit clay
				 ((or (clay? (list x (+ y 1)))
				      (water? (list x (+ y 1))))
				  (water! (list x y))
				  (fill-right (+ x 1) y done))
				 (#t
				  ;;spillover
				  ;; (water! (list x y))
				  ;; loop back to vert routine from this point
				  (vert x y)
				  ;; nothing more to do so save unwinding stack
				  ;; just cut it off at knees
				  (done #f)))))

	    
	    (let ((sprinkler-x 500)(sprinkler-y 0))
	      (vert sprinkler-x (+ 1 sprinkler-y))
	      (@show-grid)
	      #t)))))))


  




