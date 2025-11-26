
(import (chicken format)) ;; format 
(import (chicken pretty-print)) 
(import srfi-1)
(import srfi-69)
;;(import bind)
(import bindings) ;; destructuring

(define *max-count* 4352)
(define *run-count* 0)
(define *endpoints* #f)


(define id
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))
  
(define (loading)
  (format #t "loading node ~a / ~a ~%" (id) *max-count*))

(define (report)
  (format #t "running node ~a / ~a ~%" (begin (set! *run-count* (+ 1 *run-count*)) *run-count*) *max-count*))


;; each step we take gets entered into this hash table
;; initially at start position - we have taken no steps
;; the coordinate uses aoc 2 step for movement - then we can record the door
;; orientation .
;; checked after compilation that there is indeed no door that is orientated
;; in both directions.
(define *step-count* 0) 
(define *step-hash* (make-hash-table))

;; if we find a lower value n at x y - mark this square - otherwise continue nop
(define (mark-square! x y n)
  (let ((out (hash-table-ref/default *step-hash* (list x y) #f)))
    (when
	(or (not out)
	    (and (integer? out) (< n out)))
      (hash-table-set! *step-hash* (list x y) n))))


;; current no open doors 
(define *side-doors* (make-hash-table))
(define *trap-doors* (make-hash-table))


(define (open-side-door! x y)
  (hash-table-set! *side-doors* (make-point x y) #t))

(define (open-trap-door! x y)
  (hash-table-set! *trap-doors* (make-point x y) #t))


(define-record-type point
  (make-point x y)
  point?
  (x point-x point-x-set!)
  (y point-y point-y-set!))

(define-record-printer (point rt out)
  (fprintf out "#,(point ~s ~s)"  (point-x rt) (point-y rt)))


(define-record-type union
  (make-union ht)
  union?
  (ht union-ht))

(define (union-add un pts)
  (let ((ht (union-ht un)))
    (let loop ((pts pts))
      (cond
       ((null? pts) un)
       (#t (let* ((pt (car pts))
		  (found (hash-table-ref/default ht pt #f)))
	     (cond
	      (found #f)
	      (#t (hash-table-set! ht pt #t))))
	   (loop (cdr pts)))))))

(define (union-points un)
  (let ((ht (union-ht un)))
    (hash-table-map ht (lambda (k v) k))))

(define-record-printer (union rt out)
  (fprintf out "#,(union ~s)"  (union-points rt)))

;;
(define (run)
  (set! *endpoints* (t1 (list (make-point 0 0)))))

