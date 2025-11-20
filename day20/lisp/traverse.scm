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


(define demo '(SEQ a (ALT (ALT b c) d) e))


(define trav
  (lambda (t)
    (cond
     ((symbol? t)
      (cond
       ((eq? t '?) "")
       (#t (format #f "~a" t))))
     ((pair? t)
      (let ((op (car t)))
	(cond
	 ((equal? op 'SEQ)	  
	  (trav-seq t))
	 ((equal? op 'ALT)	  
	  (trav-alt t))
	 (#t (error "unknwon")))))
     (#t (error "not symbol or pair .")))))


(define (trav-seq t)  
  (let ((es (cdr t))
	(res '()))
    (let loop ((es es))
      (cond
       ((null? es) (reverse res))
       (#t (let ((e (car es)))
	     (set! res (cons (trav e) res))
	     (loop (cdr es))))))))
	

(define (trav-alt t)  
  (let ((es (cdr t))
	(res '()))
    (let loop ((es es))
      (cond
       ((null? es) (reverse res))
       (#t (let ((e (car es)))
	     (set! res (cons (trav e) res))
	     (loop (cdr es))))))))
	

(trav demo)



