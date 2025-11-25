;; instead of append we could keep a global index i and simulate we wrote to {i}
;;
;; csc -O3 -o traverse traverse.scm
;; 
;;
;; chicken scheme
(import (chicken format)) ;; format 

(define (path)
  (with-input-from-file "output/tree.lisp"
    (lambda ()
      (read))))

(define input (path))

;; may be multiple different paths
;; as one alt route may lead down multiple alt routes 
;; 

(define output-vector #f)

(define (write-string s i)
  (let ((n (+ i (string-length s))))
    n))

(define stack '())
;; (define (push! s x) (set! stack (cons x stack)))
;; (define (pop! s) (let ((top (car stack)))
;; 		(set! stack (cdr stack))
;; 		top))


;; (define (trav-alt2 alts pr k)
;;   (let loop ((alts (cdr tree)))
;;     (cond
;;      ((null? alts) (k pr))
;;      (#t (let ((alt (car alts)))
;; 	   (trav alt pr (lambda (r1)
;; 			  (loop (cdr alts)))))))))


;; (define (trav-alt tree pr k)
;;   (let ((alts (cdr tree)))
;;     (trav-alt2 alts pr k)))

;; (define (trav-alt2 alts pr k)
;;   (let loop ((alts (cdr tree)))
;;     (cond
;;      ((null? alts) (k pr))
;;      (#t (let ((alt (car alts)))
;; 	   (trav alt pr (lambda (r1)
;; 			  (loop (cdr alts)))))))))


;; (define (trav-seq tree pr k)  
;;   (let ((seqs (cdr tree)))
;;     (trav-seq2 seqs pr k)))

;; (define (trav-seq2 seqs pr k)
;;   (cond
;;    ((null? seqs) (k pr))
;;    (#t (let ((elem (car seqs)))
;; 	 (trav elem pr (lambda (r1))
;; 	       (trav2-seq (cdr seqs) r1 k))))))


;; (define (seq tree)
;;   (let loop ((elems (cdr tree)))
;;     (cond
;;      ((null? elems) nil)
;;      (#t (seq tree elems) ??))))


(define demo1 '(SEQ a (ALT (ALT b c) d ?) e))
(define demo2 '(SEQ (SEQ a (SEQ (SEQ b c d) e f) g h (SEQ i (SEQ j (SEQ k l m) o p)))))
(define demo3 '(ALT a (ALT (ALT b c) d ?) e))
(define demo4 '(ALT a (SEQ (ALT b c) d e)))
(define demo5 '(ALT a (SEQ (ALT b c))))
(define demo6 '(SEQ (ALT a b) (ALT c d) (ALT d e)))
(define demo7 '(SEQ (ALT a b) (ALT c d) (ALT d ?)))
(define demo8 '(SEQ (ALT (ALT a b c) (ALT d e f)) (ALT g h) (ALT i ?)))
(define demo9 '(ALT (ALT a) (ALT d)))
(define demo10 '(SEQ (ALT (ALT a b) (ALT c d))))
(define demo11 '(ALT ? a))
(define demo12 '(SEQ A (ALT ? a) B))


;; instead of list cons append - we simply increment an index i 
;;trav t:tree i:index k:cont  next:future
(define trav
  (lambda (t i k next)
    (cond
     ((symbol? t)
      ;; (format #t "trav : t => ~a ~%" t)
      (cond
       ((eq? t '?) (k i next))
       (#t (k (let ((slen (string-length (format #f "~a" t))))
		;; (format #t "trav.slen => ~a ~%" slen)
		(+ i slen))
	      next))))
     ((pair? t)
      (let ((op (car t)))
	(cond
	 ((equal? op 'SEQ)	  
	  (trav-seq t i k next))
	 ((equal? op 'ALT)	  
	  (trav-alt t i k next))
	 (#t (error "unknwon")))))
     (#t (error "not symbol or pair .")))))


(define (trav-seq t i k next)  
  (let ((es (cdr t))
	(acc '()))
    (trav-seq2 es i k next)))

;; sequence only has one execution path
;; whereas alt will execute k more times 
(define (trav-seq2 es i k next)
  (cond
   ((null? es) (k i next))
   (#t (let ((e (car es)))
	 (trav e i (lambda (i2 next)
		     (trav-seq2 (cdr es) i2 k next))
	       next)))))
	

(define callbacks '())

;; ;; alternate path does not have an accumulator
;; (define (trav-alt t r k next)
;;   ;; (format #t "alt: ~a " t)  
;;   (let ((es (cdr t)))
;;     (trav-alt2 es r k (lambda () #f))))
;; (define (trav-alt2 es r k next)
;;   ;; (format #t "alt2: ~a " es)
;;   (cond
;;    ((null? es) #f) 
;;    (#t (let ((e (car es)))
;; 	 (format #t "meeting => ~a ~%" e)	 
;; 	 (trav e r 
;; 	       (lambda (r2 next) ; ignore ? case for now
;; 		     ;; handle ? case - empty string
;; 		     (cond
;; 		      ((and (pair? r2) (string? (car r2)) (string=? "" (car r2))) (k r))
;; 		      (#t (k (append r r2) next))))
;; 	       next)))))

(define (trav-alt t i k next)
  (let loop ((alts (cdr t)))
      (cond
       ((null? alts) #f)
       (#t (let ((alt (car alts)))
	     ;; (format #t "trav-alt => {~a} : alts => {~a}~%" alt alts)
	     (trav alt i (lambda (i2 next)
			   (cond
			    ((and (string? alt) (string=? "" (car alt)))
			     (k i next))
			    (#t
			     (k i2 next))))
		   next)
	     (loop (cdr alts)))))))


    ;; ;; add this location onto callbacks
    ;; (call/cc (lambda (return)
    ;; 	       (set! callbacks (cons return callbacks))))

		     



(define (run d)
  (format #t "input => ~a ~%" d)
  (set! callbacks '())
  (let ((tot 0)
	(i 0)
	(db #t) ;;debug flag
	)
    (trav d i (lambda (r next)
		(set! tot (+ tot 1))
		(cond
		 (db (when (zero? (modulo tot 1000000))
		       (format #t "r {~a} => {~a} ~% " tot r)))
		 ((not db) #f)))
	  (lambda ()
	    ;;(format #t "we are next !~%")
	    #t
	    ))
    ;;(format #t "there are ~a callbacks ~%" (length callbacks))
    (format #t "there were ~a in total ~%" tot)))



(define (next)
  (cond
   ((null? callbacks) #f)
   (#t (let ((top (car callbacks)))
	 (set! callbacks (cdr callbacks))
	 (cond
	  ((procedure? top) (top #t))
	  (#t (format #t "callback not a procedure!")))))))



  

  ;; (let ((result ))
  ;;   (format #t "result => ~a~%" result)))

  ;; (let ((es (cdr t))
  ;; 	(res '()))
  ;;   (let loop ((es es))
  ;;     (cond
  ;;      ((null? es) (reverse res))
  ;;      (#t (let ((e (car es)))
  ;; 	     (set! res (cons (trav e) res))
  ;; 	     (loop (cdr es))))))))

;; (run demo3)
;; (run demo2)
;; (run demo)


;; conditional 
(run input)






