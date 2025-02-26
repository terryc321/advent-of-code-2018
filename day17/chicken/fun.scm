
(import (chicken format)) ;; format 
(import (chicken io)) ;; io read-line
(import (chicken process)) ;; ???
;; (import (chicken documentation))
(import (chicken process-context)) ;; current-directory / change-directory
(import (chicken pretty-print)) ;; pp 

(import regex) ;; string-match 

(import bindings) ;; bind

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
	 (t (error (format nil "no match on line ~a" s))))))))

(define range
  (lambda (expr)
    (cond
     ((eq? (car expr) 'x-y-y) (apply range-x-y-y (cdr expr)))
     ((eq? (car expr) 'y-x-x) (apply range-y-x-x (cdr expr)))
     (#t (error (format nil "range : bad x-y-y or y-x-x ~a in ~a" (car expr) expr))))))

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

(define clay
  (lambda (points)
    (lambda (pt)
      (let ((out (member pt points)))
	(if out #t #f)))))

(define test-1
  (lambda ()
    (let ((f (clay input)))
      (map f input))))

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



;; show grid

;; some thing to figure out




;; regex
;; (define regex1 (regexp "([0-9]+)"))

;;(string-match regex1 (car example))

;; (string-match "x=([0-9]+)" (car example))
;; (string-match "x=([0-9]+).*y=([0-9]+)[.][.]([0-9]+)" (list-ref example 0))
;; (string-match "y=([0-9]+).*x=([0-9]+)[.][.]([0-9]+)" (list-ref example 1))


