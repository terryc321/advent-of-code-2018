;;-*- geiser-scheme-implementation: guile -*-

(define (factorial n)
  (cond
   ((< n 2) (/ n 0))
   (#t (* n (factorial (- n 1))))))


;;(factorial 5)
;; read file as a big long string?

(define (get-input)
  (call-with-input-file "../input.txt"
    (lambda (port)
      (let loop ((chars '()))
	(let ((ch (read-char port)))
	  (cond
	   ((eof-object? ch) (reverse chars))
	   ((char=? ch #\newline) (loop chars))
	   ((char=? ch #\^) (loop chars))
	   ((char=? ch #\$) (loop chars))	   
	   (#t (loop (cons ch chars)))))))))


;; parser for grammar
;; N E S W
;; ( )
;; |

(define  input (list->vector (get-input)))
;; input is 14198 chars long - exclusing ^ $ and newlines
;; is there the functional equivalent of this
;; cdr would advance
;; 

;; i  index into string
;; pv parse vector
;; pvlen parse vector length
;; pstack parse stack 
(define i 0)
(define pv #f)
(define pvlen 0)
(define pstack #f)

(define (advance!)
  (set! i (+ i 1)))

(define (push-stack! s)
  (set! pstack (cons s pstack)))

(define (pop-stack!)
  (let ((top (car pstack)))
    (set! pstack (cdr pstack))
    top))

(define (is-open-parens?)
  (cond
   ((and (< i pvlen) (char=? (vector-ref pv i) #\( )) #t)
   (#t #f)))

(define (is-close-parens?)
  (cond
   ((and (< i pvlen) (char=? (vector-ref pv i) #\) )) #t)
   (#t #f)))

(define (is-alternative?)
  (cond
   ((char=? (vector-ref pv i) #\| ) #t)
   (#t #f)))

(define (is-direction?)
  (cond
   ((< i pvlen) (let ((ch (vector-ref pv i)))
		  (cond
		   ((char=? ch #\N) #t)
		   ((char=? ch #\S) #t)
		   ((char=? ch #\E) #t)
		   ((char=? ch #\W) #t)
		   (#t #f))))
   (#t #f)))

(define (get-current-char)
  (vector-ref pv i))

;; a | b | c | d ... 
(define (parse-alt)
  #t)

;; ==========  N S E W  ================================
(define (parse-direction)
  (cond
   ((is-direction?) (push-stack! (vector-ref pv i))
    (advance!))
   (#t #f)))

(define (parse-word)
  (format #t "parse-word => ~a~%" (get-current-char))
  (cond
   ((is-direction?) (let loop ((chars '()))
	     (cond
	      ((is-direction?)
	       (parse-direction)
	       (let ((ch (pop-stack!)))
		 (format #t "parse-word : ch on stack was ~a ~%" ch)
		 (loop (cons ch chars))))
	      (#t (push-stack! (reverse chars))))))
   (#t #f)))

;; ==========  N S E W  ================================

;; (define (parse-parens)
;;   (cond
;;    ((is-open-parens?)
;;     (advance)
;;     (parse-parens)
;;     (let loop ((items '()))
;;       (cond
;;        ((dir?) (parse-direction)
;; 	       (let ((ch (pop-stack!)))
;; 		 (loop (cons ch chars))))
;; 	      (#t (push-stack! (reverse chars))))))
;;     (cond
;;      ((is-close-parens?) (advance!))
;;      (#t 

(define (parse item)
  (let ((tmp-vec (cond
		  ((string? item) (list->vector (string->list item)))
		  ((vector? item) item))))
    (set! i 0)
    (set! pv tmp-vec)
    (set! pvlen (vector-length tmp-vec))
    (set! pstack '())
    (parse-word)
    (pop-stack!)))


(parse "ASDF")

