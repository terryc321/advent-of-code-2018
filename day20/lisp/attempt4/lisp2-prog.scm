
(import (chicken format)) ;; format 
(import (chicken pretty-print)) 
(import srfi-1)
(import srfi-69)
;;(import bind)
(import bindings) ;; destructuring

(define *max-count* 4352)
(define *run-count* 0)
(define *endpoints* #f)


(define (make-2d-array cols rows  init)
  (let ((v (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((= i rows) v)
      (vector-set! v i (make-vector cols init)))))

(define (array-ref2 a i j)
  (vector-ref (vector-ref a (- j 1)) (- i 1)))

(define (array-set2! a i j val)
  (vector-set! (vector-ref a (- j 1)) (- i 1) val))

(define *step-array* (make-2d-array 2400 2400 #f))

;; each step we take gets entered into this hash table
;; initially at start position - we have taken no steps
;; the coordinate uses aoc 2 step for movement - then we can record the door
;; orientation .
;; checked after compilation that there is indeed no door that is orientated
;; in both directions.
(define *step-count* 0)
;; rather *step-hash* use a literal big array - how big ?
;; we can simply run though the 
;; (define *step-hash* (make-hash-table))

;; if we find a lower value n at x y - mark this square - otherwise continue nop
;; hopefully we dont choke with out of bounds !
(define (verify-step-array)
  (format #t "*step-array* read and write test ... ")
  (do ((y 1 (+ y 1)))
      ((>= y 2400) #f)
    (do ((x 1 (+ x 1)))
	((>= x 2400) #f)
      (let ((out (array-ref2 *step-array* x y)))
	(array-set2! *step-array* x y #f))))
  ;; we passed if we reach this far
  (format #t "passed ~%"))


;; activate the check !
(verify-step-array)


;; real coordinate (0,0) ...... becomes (1200,1200) in ARRAY encoding
;;                        <<<..... decoding we need subtract 1200,1200
(define (translate-in-x x)  (+ x 1200))
(define (translate-in-y y)  (+ y 1200))
(define (translate-out-x x)  (- x 1200))
(define (translate-out-y y)  (- y 1200))


(define (mark-square! x y n)
  (let* ((tx (translate-in-x x))
	 (ty (translate-in-y y))
	 (out (array-ref2 *step-array* tx ty)))
    (when
	(or (not out)
	    (and (integer? out) (< n out)))
      ;; (format #t "marking square ~a ~a with ~a : writing array at ~a ~a ~%" x y n tx ty)
      ;; (when (> n 30)
      ;; 	(format #t "enter something to continue ...~%")
      ;; 	(read))
      (array-set2! *step-array* tx ty n))))



(define id
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))
  
(define (loading)
  (format #t "loading node ~a / ~a ~%" (id) *max-count*))

(define (report)
  (format #t "running node ~a / ~a ~%" (begin (set! *run-count* (+ 1 *run-count*)) *run-count*) *max-count*))



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


;; just run it - get idea of run time
(run)

;; report all open doors
(call-with-output-file "../output/side-doors.scm"
  (lambda (port)
    (format port "(define *side-doors* '(")
    (hash-table-for-each *side-doors* (lambda (k v)
				   (format port "(~a ~a)~%" (point-x k) (point-y k))))
    (format port "))~%")))
(format #t "recorded *side-doors* in ../output/side-doors.scm~%")


(call-with-output-file "../output/trap-doors.scm"
  (lambda (port)
    (format port "(define *trap-doors* '(")
    (hash-table-for-each *trap-doors* (lambda (k v)
				   (format port "(~a ~a)~%" (point-x k) (point-y k))))
    (format port "))~%")))
(format #t "recorded *trap-doors* in ../output/trap-doors.scm~%")


(call-with-output-file "../output/step-array.scm"
  (lambda (port)
    (format port "(define *step-array* '(")
    (do ((y 0 (+ y 1)))
	((>= y 2399) #f)
      (do ((x 0 (+ x 1)))
	  ((>= x 2399) #f)
	(let ((tx (translate-out-x x))
	      (ty (translate-out-y y)))
	  (format port "(~a ~a ~a)~%" x y (array-ref2 *step-array* tx ty)))))
    (format port "))~%")))
(format #t "recorded *step-array* in ../output/step-array.scm~%")

 
(call-with-output-file "../output/end-points.scm"
  (lambda (port)
    (format port "(define *endpoints* '(")
    (let loop ((pts *endpoints*))
      (cond
       ((null? pts) #f)
       (#t (let ((pt (car pts)))
	     (format port "(~a ~a)~%" (point-x pt) (point-y pt))
	     (loop (cdr pts))))))
    (format port "))~%")))
(format #t "recorded *endpoints* in ../output/end-points.scm~%")

    
