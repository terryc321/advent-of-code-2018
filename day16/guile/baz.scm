
;; aoc 2018 day16 

(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))

(define pp pretty-print)

;;(chdir "day16/guile")
;;(getcwd)

;; read program in on one go 
(define *program* (call-with-input-file "prog.scm"
		  (lambda (port)
		    (read port))))

(define g/reg-or-imm
  (lambda (fn str)
    (lambda (op a b c r0 r1 r2 r3)
      (let* ((x (cond
		 ((= a 0) r0)
		 ((= a 1) r1)
		 ((= a 2) r2)
		 ((= a 3) r3)
		 (#t (error (format #f "~a no a" str)))))
	     (y (cond ;; value of b
		 ((= b 0) r0)
		 ((= b 1) r1)
		 ((= b 2) r2)
		 ((= b 3) r3)
		 (#t (error (format #f "~a no b" str)))))
	     (out (fn x y a b)))
	(cond
	 ((= c 0) (list out r1 r2 r3))
	 ((= c 1) (list r0 out r2 r3))
	 ((= c 2) (list r0 r1 out r3))
	 ((= c 3) (list r0 r1 r2 out ))
	 (#t (error (format #f "~a no c" str))))))))

(define g/addr (g/reg-or-imm (lambda (ra rb va vb) (+ ra rb)) "g/addr")) ;--
(define g/addi (g/reg-or-imm (lambda (ra rb va vb) (+ ra vb)) "g/addi")) ;--
(define g/mulr (g/reg-or-imm (lambda (ra rb va vb) (* ra rb)) "g/mulr")) ;-
(define g/muli (g/reg-or-imm (lambda (ra rb va vb) (* ra vb)) "g/muli")) ;-
(define g/banr (g/reg-or-imm (lambda (ra rb va vb) (logand ra rb))  "g/banr")) ;-
(define g/bani (g/reg-or-imm (lambda (ra rb va vb) (logand ra vb)) "g/bani")) ;-
(define g/borr (g/reg-or-imm (lambda (ra rb va vb) (logior ra rb)) "g/borr")) ;-
(define g/bori (g/reg-or-imm (lambda (ra rb va vb) (logior ra vb)) "g/bori")) ;-
(define g/setr (g/reg-or-imm (lambda (ra rb va vb) ra) "g/setr"))
(define g/seti (g/reg-or-imm (lambda (ra rb va vb) va) "g/seti"))
(define g/gtir (g/reg-or-imm (lambda (ra rb va vb) (if (> va rb) 1 0)) "g/gtir"))
(define g/gtri (g/reg-or-imm (lambda (ra rb va vb) (if (> ra vb) 1 0)) "g/gtri"))
(define g/gtrr (g/reg-or-imm (lambda (ra rb va vb) (if (> ra rb) 1 0)) "g/gtrr"))
(define g/eqir (g/reg-or-imm (lambda (ra rb va vb) (if (= va rb) 1 0)) "g/eqir"))
(define g/eqri (g/reg-or-imm (lambda (ra rb va vb) (if (= ra vb) 1 0)) "g/eqri"))
(define g/eqrr (g/reg-or-imm (lambda (ra rb va vb) (if (= ra rb) 1 0)) "g/eqrr"))

(define process
  (let ((alist `(( 0 ,g/eqri   )
		 ( 1 ,g/mulr   )
		 ( 2 ,g/gtri   )
		 ( 3 ,g/gtrr   )
		 ( 4 ,g/banr   )
		 ( 5 ,g/addi   )
		 ( 6 ,g/seti   )
		 ( 7 ,g/gtir   )
		 ( 8 ,g/muli   )
		 ( 9 ,g/bori   )
		 ( 10 ,g/setr   )
		 ( 11 ,g/addr   )
		 ( 12 ,g/bani   )
		 ( 13 ,g/borr   )
		 ( 14 ,g/eqir   )
		 ( 15 ,g/eqrr   ))))
  (lambda (xs r0 r1 r2 r3)
    (cond
     ((null? xs) `((r0 ,r0) (r1 ,r1) (r2 ,r2) (r3 ,r3)))
     (#t (let ((cmd (car xs)))
	   (match cmd
	     ((op a b c)
	      (let ((func (second (assoc op alist))))
		(let ((out (func op a b c r0 r1 r2 r3)))
		  (format #t "out => ~a ~%" out)
		  (match out
		    ((r0 r1 r2 r3) (process (cdr xs) r0 r1 r2 r3))
		    (_ (error "process-out no match"))))))
	     (_ (error "process no match")))))))))



     
(define run
  (lambda ()
    (let ((r0 0)
	  (r1 0)
	  (r2 0)
	  (r3 0))
      (process *program* r0 r1 r2 r3))))

#|
> (run)

...
$15 = ((r0 653) (r1 1) (r2 4) (r3 653))
           ^^^^

value left in register 0 is ......... 653 ....... ACCEPTED .....



|#
