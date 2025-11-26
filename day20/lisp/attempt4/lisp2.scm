;; revert back to common lisp
;;
;; chicken scheme
(import (chicken format)) ;; format 
(import (chicken pretty-print)) 
(import srfi-1)
(import srfi-69)
;;(import bind)
(import bindings) ;; destructuring

(define (load-tree fname)
  (with-input-from-file fname
    (lambda ()
      (read))))

(define (tree)
  (format #t "loading tree ...")
  (let ((t (load-tree "tree.lisp")))
    (format #t " done.~%")
    t))



(define *stdout* #t) 

;; point datatype

(define-record-type point
  (make-point x y)
  point?
  (x point-x point-x-set!)
  (y point-y point-y-set!))

(define-record-printer (point rt out)
  (fprintf out "#,(point ~s ~s)"  (point-x rt) (point-y rt)))


;; node datatype 

(define-record-type node
  (make-node id type leafs data)
  node?
  (id node-id node-id-set!)
  (type node-type node-type-set!)
  (leafs node-leafs node-leafs-set!)
  (data node-data node-data-set!)
)


(define-record-printer (node rt out)
  (cond
   ((eq? (node-type rt) 'str)
      (fprintf out "#,(node[str] ~s ~s)"  (node-id rt) (node-data rt)))
   ((eq? (node-type rt) 'seq)
      (fprintf out "#,(node[seq] ~s ~s)"  (node-id rt) (node-leafs rt)))
   ((eq? (node-type rt) 'alt)
      (fprintf out "#,(node[alt] ~s ~s)"  (node-id rt) (node-leafs rt)))
   (#t (error "unknown node type"))))

(define (node-str? n) (eq? (node-type n) 'str))
(define (node-seq? n) (eq? (node-type n) 'seq))
(define (node-alt? n) (eq? (node-type n) 'alt))
  

;; union types 

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


;; if we keep track of which direction i move i can use a smaller grid ?
(define *board* (make-2d-array 2400 2400 #f))
;; array-ref2  *board* x y    : x,y 1 based index to 2400 inclusive
;; array-set2! *board* x y n  : x,y 1 based index to 2400 inclusive

(define (verify-board)
  (format #t "*step-array* read and write test ... ")
  (do ((y 1 (+ y 1)))
      ((>= y 2400) #f)
    (do ((x 1 (+ x 1)))
	((>= x 2400) #f)
      (let ((out (array-ref2 *board* x y)))
	(array-set2! *board* x y #f))))
  ;; we passed if we reach this far
  (format #t "passed ~%"))


;; real coordinate (0,0) ...... becomes (1200,1200) in ARRAY encoding
;;                        <<<..... decoding we need subtract 1200,1200
(define (translate-in-x x)  (+ x 1200))
(define (translate-in-y y)  (+ y 1200))
(define (translate-out-x x)  (- x 1200))
(define (translate-out-y y)  (- y 1200))

;; mark-board! simply writes data to grid square x y
(define (mark-board! x y data)
  (let* ((tx (translate-in-x x))
	 (ty (translate-in-y y)))
    (array-set2! *board* tx ty data)))

;; should we check if we try to overwrite something that is already there ?
(define (open-side-door! x y)  
  (mark-board! x y #\| ))

(define (open-trap-door! x y)
  (mark-board! x y #\_ ))

(define *node-count* 0)

;; given set of points -> return another set of points 
(define (trav t pts)
  ;; some visual cue something is happening
  (set! *node-count* (+ 1 *node-count*))
  (format #t "processing node ~a ~%" *node-count*)
  (cond
   ((symbol? t)
    (trav-symbol t pts))
   ((pair? t)
    (let ((op (car t)))
      (cond
       ((equal? op 'SEQ)	  
	(trav-seq t pts))
       ((equal? op 'ALT)	  
	(trav-alt t pts))
       (#t (error (format #f "unknwon ~a " t))))))
   (#t (error "not symbol or pair ~a." t))))

(define (trav-seq t p)
  (let loop ((es (cdr t)) (pts p))
    (cond
     ((null? es) pts)
     (#t (let ((e (car es)))
	   (let ((pts2 (trav e pts)))	     
	     (loop (cdr es) pts2)))))))


(define (trav-alt t pts)
  (let ((un (make-union (make-hash-table))))
    (let loop ((es (cdr t)))
      (cond
       ((null? es) (union-points un))
       (#t (let ((e (car es)))
	     (let ((out (trav e pts)))
	       (set! un (union-add un out))
	       (loop (cdr es)))))))))

(define (trav-symbol t pts)
  (let* ((s (format #f "~a" t))
	 (slen (string-length s)))
    (cond
       ((string=? s "?") pts) ;nop
       (#t
	(letrec ((foo (lambda (pt)
			(let ((x (point-x pt))(y (point-y pt)))
			  (do ((i 0 (+ i 1)))
			      ((>= i slen) pts)
			    (let ((ch (string-ref s i)))
			      (cond
			       ((char=? ch #\N)
				(open-trap-door! x (+ y 1))
				(set! y (+ y 2))
				(mark-board! x y #\.))
			       ((char=? ch #\S)
				(open-trap-door! x (+ y -1))
				(set! y (+ y -2))
				(mark-board! x y #\.))
			       ((char=? ch #\E)
				(open-side-door! (+ x 1) y)
				(set! x (+ x 2))
				(mark-board! x y #\.))
			       ((char=? ch #\W)
				(open-side-door! (+ x -1) y)
				(set! x (+ x -2))
				(mark-board! x y #\.))
			       (#t (error (format #f "trav-symbol s=~a error - case not handled" s))))))
			  (make-point x y)))))
	  (map foo pts))))))


;; anywhere else is a wall
(define (report)
  (format #t "recording board data ... ")
  (call-with-output-file "results.scm"
    (lambda (port)
      (format port "(define board '(")
      (do ((y 1 (+ y 1)))
	  ((>= y 2400) #f)
	(do ((x 1 (+ x 1)))
	    ((>= x 2400) #f)
	  (let* ((val (array-ref2 *board* x y))
		 (cx (translate-out-x x))
		 (cy (translate-out-y y)))
	    (cond
	     ((not val) #f) 
	     (#t (format port "(~a ~a #\~a)~%" cx cy val))))))
      (format port "))~%")))
  (format #t "board data written ...~%"))



(define (run)
  (verify-board)   
  (let ((t (tree))
	(pts (list (make-point 0 0))))
    (mark-board! 0 0 #\X)
    (trav t pts)
    (report)))


(run)


