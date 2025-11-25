;; revert back to common lisp
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

(define *stdout* #f)

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
     ((eq? t '?) (format *stdout* "\"?\""))
     (#t (format *stdout* "\"~a\" " t))))
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
  (format *stdout* "(seq ")
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) #f)
     (#t (let ((leaf (car leafs)))
	   (trav leaf pts)
	   (when (not (null? (cdr leafs)))
	     (format *stdout* " "))
	   (loop (cdr leafs) pts)))))
  (format *stdout* ")"))


(define (trav-alt t pts)
  (format *stdout* "(alt ")
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) #f)
     (#t (let ((leaf (car leafs)))
	   (trav leaf pts)
	   (when (not (null? (cdr leafs)))
	     (format *stdout* " "))
	   (loop (cdr leafs) pts)))))
  (format *stdout* ")"))


(define (go)
  (call-with-output-file "lisp.out"
    (lambda (port)
      (set! *stdout* port)	
      ;; (format port "(* type decl *)~%")
      ;; (format port "type tree = TSeq of tree list | TStr of string | TAlt of tree list ;; ~%")
      ;; (format port "(* parse tree itself - careful in is a reserved word ! *)~%")
      (format port "(defparameter *input* = ")  
      (trav input '())
      (format port ")~%"))))




