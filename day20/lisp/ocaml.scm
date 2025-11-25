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
     ((eq? t '?) (format *stdout* "(TStr \"?\") "))
     (#t (format *stdout* "(TStr \"~a\") " t))))
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
  (format *stdout* "(TSeq [")
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) #f)
     (#t (let ((leaf (car leafs)))
	   (trav leaf pts)
	   (when (not (null? (cdr leafs)))
	     (format *stdout* " ; "))
	   (loop (cdr leafs) pts)))))
  (format *stdout* "])"))


(define (trav-alt t pts)
  (format *stdout* "(TAlt [")
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) #f)
     (#t (let ((leaf (car leafs)))
	   (trav leaf pts)
	   (when (not (null? (cdr leafs)))
	     (format *stdout* " ; "))
	   (loop (cdr leafs) pts)))))
  (format *stdout* "])"))


(define (go)
  (call-with-output-file "../ocaml/fun.ml"
    (lambda (port)
      (set! *stdout* port)	
      (format port "(* type decl *)~%")
      (format port "type tree = TSeq of tree list | TStr of string | TAlt of tree list ;; ~%")
      (format port "(* parse tree itself - careful in is a reserved word ! *)~%")
      (format port "let ins = ")  
      (trav input '())
      (format port ";;~%"))))



