;; csc -O3 -o traverse traverse.scm
;;
;;
;; chicken scheme
(import (chicken format)) ;; format 
(import (chicken pretty-print)) 
(import srfi-1)
;;(import bind)
(import bindings) ;; destructuring

(define (path)
  (with-input-from-file "output/tree.lisp"
    (lambda ()
      (read))))

(define input (path))



(define (north! points len)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 1 (+ (vector-ref vf 1) 1))
	   )))))
        
(define (south! points len)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 1 (- (vector-ref vf 1) 1))
	   )))))
  
(define (east! points len)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 0 (+ (vector-ref vf 0) 1))
	   )))))
    
(define (west! points len)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 0 (- (vector-ref vf 0) 1))
	   )))))
  
  
;; traverse :: tree -> points -> points 

(define (trav t pts)
  (cond
   ((symbol? t)
    (cond
     ((eq? t '?) pts)
     (#t (let* ((str (format #f "~a" t))
		(len (string-length str))
		(v (list->vector (map (lambda (x) (list->vector x)) pts)))
		(vlen (vector-length v))
		(x 0)
		(y 0))
	   (cond
	    ((string=? str "?") pts)
	    (#t 
	     (let loop ((i 0))
	       (cond
		((>= i len) (map vector->list (vector->list v)))
		(#t
		 (let ((ch (string-ref str i)))
		   (cond
		    ((char=? ch #\N) (north! v vlen) (loop (+ i 1)))
		    ((char=? ch #\S) (south! v vlen) (loop (+ i 1)))
		    ((char=? ch #\E) (east! v vlen) (loop (+ i 1)))
		    ((char=? ch #\W) (west! v vlen) (loop (+ i 1)))
		    (#t (error "move")))))))))))))
   ((pair? t)
    (let ((op (car t)))
      (cond
       ((equal? op 'SEQ)	  
	(trav-seq t pts))
       ((equal? op 'ALT)	  
	(trav-alt t pts))
       (#t (error "unknwon")))))
   (#t (error "not symbol or pair ."))))


(define (trav-seq t pts)
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) points)
     (#t (let ((leaf (car leafs)))
	   (let ((new-points (trav leaf points)))
	     ;; (format #t "processed in : ~a : => ~a ~%" pts leaf new-points)
	     (loop (cdr leafs) new-points)))))))


(define (trav-alt t pts)
  (let ((union '()))
    (let loop ((leafs (cdr t)))
      (cond
       ((null? leafs) union)
       (#t (let ((leaf (car leafs)))
	     (let ((out (trav leaf pts)))
	       (set! union (append out union))
	       (loop (cdr leafs)))))))))


;; given some initial point 0,0
(define (run)
  (trav input (list '(0 0))))

;;(run)


