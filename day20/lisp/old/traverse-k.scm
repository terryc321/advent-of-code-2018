;;  -*- geiser-implementation: guile *-*

(define (path)
  (with-input-from-file "output/tree.lisp"
    (lambda ()
      (read))))

(define input (path))

;; may be multiple different paths
;; as one alt route may lead down multiple alt routes 
;; 


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


(define demo '(SEQ a (ALT (ALT b c) d ?) e))
(define demo2 '(SEQ (SEQ a (SEQ (SEQ b c d) e f) g h (SEQ i (SEQ j (SEQ k l m) o p)))))
(define demo3 '(ALT a (ALT (ALT b c) d ?) e))


(define trav
  (lambda (t r k)
    (cond
     ((symbol? t)
      (cond
       ((eq? t '?) (k (list "")))
       (#t (k (list t)))))
     ((pair? t)
      (let ((op (car t)))
	(cond
	 ((equal? op 'SEQ)	  
	  (trav-seq t r k))
	 ((equal? op 'ALT)	  
	  (trav-alt t r k))
	 (#t (error "unknwon")))))
     (#t (error "not symbol or pair .")))))


(define (trav-seq t r k)  
  (let ((es (cdr t))
	(acc '()))
    (trav-seq2 es acc r k)))

;; sequence only has one execution path
;; whereas alt will execute k more times 
(define (trav-seq2 es acc r k)
  (cond
   ((null? es) (k (append r acc)))
   (#t (let ((e (car es)))
	 (trav e r (lambda (r2)
		     (trav-seq2 (cdr es) (append acc r2) r k)))))))
	

(define callbacks '())

;; alternate path does not have an accumulator
(define (trav-alt t r k)
  ;; (format #t "alt: ~a " t)  
  (let ((es (cdr t)))
    (trav-alt2 es r k)))

(define (trav-alt2 es r k)
  ;; (format #t "alt2: ~a " es)
  (cond
   ((null? es) #f) 
   (#t (let ((e (car es)))
	 (trav e r (lambda (r2) ; ignore ? case for now
		     ;; handle ? case - empty string
		     (cond
		      ((and (pair? r2) (string? (car r2)) (string= "" (car r2))) (k r))
		      (#t (k (append r r2))))
		     ;; (call/cc (lambda (next)
		     ;; 		(k (append r r2))
		     ;; 		(set! callbacks (cons next callbacks))))
		     ;; (format #t "we resume here => ~%")
		     (trav-alt2 (cdr es) r k)))))))


(define (run d)
  (format #t "input => ~a ~%" d)
  ;;(set! callbacks '())
  (trav d '() (lambda (r) (format #t "r => ~a ~%" r)))
  ;;(format #t "there are ~a callbacks ~%" (length callbacks))
  )

  

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
	
;; (run demo2)
;; (run demo)



