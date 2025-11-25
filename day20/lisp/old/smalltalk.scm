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

;; lets try convert to smalltalk - maybe a change in perspective will
;; yield a solution

;; SEQ
;; ALT
;; symbols NSEW
;; symbol ?

 
(define (trav t pts)
  (cond
   ((symbol? t)
    (cond
     ((eq? t '?) (format #t "'?' "))
     (#t (format #t "'~a' " t))))
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
  (format #t "#(SEQ ")
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) #f)
     (#t (let ((leaf (car leafs)))
	   (trav leaf pts)
	   (format #t " ")
	   (loop (cdr leafs) pts)))))
  (format #t ")"))


(define (trav-alt t pts)
  (format #t "#(ALT ")
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) #f)
     (#t (let ((leaf (car leafs)))
	   (trav leaf pts)
	   (format #t " ")
	   (loop (cdr leafs) pts)))))
  (format #t ")"))


(define (go)
  (trav input '()))


