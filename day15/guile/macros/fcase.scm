
;; sudo mkdir -pv /usr/local/share/guile/site/3.0/macros
;;
;; but ive compiled own version of guile and placed it into /opt/guile
;; so in my case i need to 
;;
;; sudo cp -v fcase.scm /opt/guile/share/guile/site/macros/fcase.scm
;;
;; > guile
;; (use-modules (macros fcase))
;; (fcase 1 ((1) 2 22 222)(else 3 4 5)) => 222
;; (fcase 3 ((1) 2 22 222)(else 3 4 5)) => 5
;; (fcase 'trivial ((1) 2 22 222)(else 3 4 5)) => 5

(define-module (macros fcase)
  #:export (fcase))

;; (use-modules (language tree-il))
;; (use-modules (ice-9 pretty-print))
;; (use-modules (ice-9 format))
;; (define pp pretty-print)

(define (fcase-helper2 k xs)
  (cond
    ((null? xs) '())
    (#t 
     (let ((datums (car (car xs)))
	   (body (cdr (car xs))))
       (cond
	((or (eq? datums #t)
	     (eq? datums 'else)) `((#t ,@body)))
	 (#t (append `(((or
		       ,@(map (lambda (val) `(eqv? ,k ,val)) datums))
		       ,@body))
		     (fcase-helper2 k (cdr xs)))))))))

#|
(fcase-helper2 1 '(((1 2 3) 'a)
		  ((4 5 6) 'b)
		  ((7 8 9) 'c)
		  (else 'd)))
|#

(define (fcase-helper key clauses)
  (let ((k (gensym "key")))
    `(let ((,k ,key))
       (cond
       ,@(fcase-helper2 k clauses)))))


(defmacro fcase (key . clauses)
  (fcase-helper key clauses))


#|

guile generates intermediate language
file:///usr/share/doc/guile-3.0.10/ref/Macro-Expansion.html

(use-modules (language tree-il))
(tree-il->scheme (macroexpand '(+ 1 2)))


|#


#|
(pp
 (tree-il->scheme
  (macroexpand '(fcase 1
		       ((1 2 3) 'a)
		       ((4 5 6) 'b)
		       ((7 8 9) 'c)
		       (else 'd)))))


(fcase 11
       ((1 2 3 (+ 10 1)) 'a)
       ((4 5 6) 'b)
       ((7 8 9) 'c)
       (else 'd))
|#




