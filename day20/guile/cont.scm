;;-*- geiser-scheme-implementation: guile -*-
;;
;; cases
;; N S E W = direction
;; ( open parens
;; ) close parens
;; | alt
;; end of string
;;

;; s string
;; i int
;; len int
;; ch char
;; k bool
(define ischar? (lambda (s i len ch k)
		  (cond
		   ((< i len) (k (char=? ch (string-ref s i))))
		   (#t (k #f)))))


(define dir? (lambda (s i len k)
	       (ischar? s i len #\N
			(lambda (r)
			  (if r (k #\N)
			      (ischar? s i len #\E
				       (lambda (r)
					 (if r (k #\E)
					     (ischar? s i len #\W
						      (lambda (r)
							(if r (k #\W)
							    (ischar? s i len #\S
								     (lambda (r)
								       (if r (k #\S) (k #f)))))))))))))))


;; s i len k: bool -> list
(define open? (lambda (s i len k)
		(ischar? s i len #\( k)))

(define close? (lambda (s i len k)
		 (ischar? s i len #\) k)))

(define eof? (lambda (s i len k)
	       (k (>= i len))))


(define (parse-parens s i len k)
  (cond
   ((open?)
    (advance!)
    (let ((r (parse-seq)))
      (cond
       ((is-close-parens?) (advance!)))
      r))))
      

(define (parse-direction s i len k)
  (cond
   ((?) (list (vector-ref pv i))
    (advance!))
   (#t #f)))

(define (parse-seq s i len stk k)
  (format #t "stk => ~a ~%" stk)
  (dir? s i len (lambda (r1)
		  (format #t "found letter ~a ~%" r1)
		  (cond
		   (r1 (parse-seq s (+ i 1) len (cons (string-ref s i) stk) k))
		   (#t
		    (format #t "exit.stk => ~a ~%" (reverse stk))		    
		    (k (reverse stk))))))
  (alt? s i len (lambda (r1)
		  (cond
		   (r1 (parse-seq s (+ i 1) len (cons (string-ref s i) stk) k))


(define (parse s)
  (let ((stk '())
	(slen (string-length s))
	(init-i 0))
    (parse-seq s init-i slen stk
	       (lambda (result)
		 (format #t "result ==> ~a~%" result)))))


