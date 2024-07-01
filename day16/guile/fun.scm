
;; aoc 2018 day16 

(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))

;;(chdir "day16/guile")
;;(getcwd)

(define *samples* (call-with-input-file "samples.scm"
		  (lambda (port)
		    (read port))))

;;addr (add register) stores into register C the result of adding register A and register B.
(define f/addr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/addr no a"))))
	   (y (cond
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/addr no b"))))
	   (out (+ x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/addr no c"))))))

	  
;;addi (add immediate) stores into register C the result of adding register A and value B.
(define f/addi
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/addi no a"))))
	   (y (cond ;; value of b
	       ((= b 0) b)
	       ((= b 1) b)
	       ((= b 2) b)
	       ((= b 3) b)
	       (#t (error "f/addi no b"))))
	   (out (+ x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/addi no c"))))))


;;mulr (multiply register) stores into register C the result of multiplying register A and register B.
(define f/mulr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/mulr no a"))))
	   (y (cond
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/mulr no b"))))
	   (out (* x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/mulr no c"))))))


;;muli (multiply immediate) stores into register C the result of multiplying register A and value B.
(define f/muli
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/muli no a"))))
	   (y (cond ;; value of b
	       ((= b 0) b)
	       ((= b 1) b)
	       ((= b 2) b)
	       ((= b 3) b)
	       (#t (error "f/muli no b"))))
	   (out (* x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/muli no c"))))))



;; banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
;; bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
(define f/banr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/banr no a"))))
	   (y (cond
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/banr no b"))))
	   (out (logand x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/banr no c"))))))



(define f/bani
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/bani no a"))))
	   (y (cond ;; value of b
	       ((= b 0) b)
	       ((= b 1) b)
	       ((= b 2) b)
	       ((= b 3) b)
	       (#t (error "f/bani no b"))))
	   (out (logand x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/bani no c"))))))


    ;; borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
    ;; bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
(define f/borr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/borr no a"))))
	   (y (cond
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/borr no b"))))
	   (out (logior x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/borr no c"))))))



(define f/bori
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/bori no a"))))
	   (y (cond ;; value of b
	       ((= b 0) b)
	       ((= b 1) b)
	       ((= b 2) b)
	       ((= b 3) b)
	       (#t (error "f/bori no b"))))
	   (out (logior x y)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/bori no c"))))))



;; setr (set register) copies the contents of register A into register C. (Input B is ignored.)
;; seti (set immediate) stores value A into register C. (Input B is ignored.)
(define f/setr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/setr no a"))))
	   (y (cond
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/setr no b"))))
	   (out x))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/setr no c"))))))


(define f/seti
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/seti no a"))))
	   (y (cond ;; value of b
	       ((= b 0) b)
	       ((= b 1) b)
	       ((= b 2) b)
	       ((= b 3) b)
	       (#t (error "f/seti no b"))))
	   (out a))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/seti no c"))))))


;; gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
(define f/gtir
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/gtir no a"))))
	   (y (cond 
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/gtir no b"))))
	   (out (if (> a y) 1 0)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/gtir no c"))))))

;; gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
(define f/gtri
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/gtri no a"))))
	   (y (cond 
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/gtri no b"))))
	   (out (if (> x b) 1 0)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/gtri no c"))))))
;; gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
(define f/gtrr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/gtrr no a"))))
	   (y (cond 
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/gtrr no b"))))
	   (out (if (> x y) 1 0)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/gtrr no c"))))))


;; eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
(define f/eqir
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/eqir no a"))))
	   (y (cond 
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/eqir no b"))))
	   (out (if (= a y) 1 0)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/eqir no c"))))))
;; eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
(define f/eqri
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/eqri no a"))))
	   (y (cond 
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/eqri no b"))))
	   (out (if (= x b) 1 0)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/eqri no c"))))))

;; eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
(define f/eqrr
  (lambda (op a b c r0 r1 r2 r3)
    (let* ((x (cond
	       ((= a 0) r0)
	       ((= a 1) r1)
	       ((= a 2) r2)
	       ((= a 3) r3)
	       (#t (error "f/eqrr no a"))))
	   (y (cond 
	       ((= b 0) r0)
	       ((= b 1) r1)
	       ((= b 2) r2)
	       ((= b 3) r3)
	       (#t (error "f/eqrr no b"))))
	   (out (if (= x y) 1 0)))
      (cond
       ((= c 0) (list out r1 r2 r3))
       ((= c 1) (list r0 out r2 r3))
       ((= c 2) (list r0 r1 out r3))
       ((= c 3) (list r0 r1 r2 out ))
       (#t (error "f/eqrr no c"))))))



;; ---- a more generic version handle both register and immediate 
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
  (lambda (fn-list)
    (lambda (sample)
      (let ((before (first sample))
	    (after (third sample))
	    (command (second sample)))
	;; (format #t "before = [~a]~%" before)
	;; (format #t "after = [~a]~%" after)
	;; (format #t "command = [~a]~%" command)
	(match (append command before)
	  ((op a b c r0 r1 r2 r3)
	   (length
	    (filter
	     (lambda (x) (equal? x after))
	     (map (lambda (fn)
		    (fn op a b c r0 r1 r2 r3))
		  fn-list))))
	  (_ (error "no match")))))))


(define task
  (lambda (fn-list)
    (let ((table (map (process fn-list) *samples*)))
      (length (filter (lambda (x) (>= x 3)) table)))))

(define task2
  (lambda (fn-list)
    (let ((table (map (process fn-list) *samples*)))
      table)))


(define (demo)
  (task2 (list
	  f/addr
	  f/addi
	  f/mulr
	  f/muli
	  f/banr
	  f/bani
	  f/borr
	  f/bori
	  f/setr
	  f/seti
	  f/gtir
	  f/gtri
	  f/gtrr
	  f/eqir
	  f/eqri
	  f/eqrr
	  )))

(define (demo2)
  (task2 (list
	  g/addr
	  g/addi
	  g/mulr
	  g/muli
	  g/banr
	  g/bani
	  g/borr
	  g/bori
	  g/setr
	  g/seti	  
	  g/gtir
	  g/gtri
	  g/gtrr
	  g/eqir
	  g/eqri
	  g/eqrr	  
	  )))

(define (check)
  (map (lambda (x y z)
	 (list z (equal? (task2 (list x)) (task2 (list y)))))
       (list f/addr)
       (list g/addr)
       '(addr)))


(define check2
  (lambda (func1 title1 func2 title2)
    (lambda (sample)
      (let ((before (first sample))
	    (after (third sample))
	    (command (second sample)))
	;; (format #t "before = [~a]~%" before)
	;; (format #t "after = [~a]~%" after)
	;; (format #t "command = [~a]~%" command)
	(match (append command before)
	  ((op a b c r0 r1 r2 r3)
	   (let ((after1 (func1 op a b c r0 r1 r2 r3))
		 (after2 (func2 op a b c r0 r1 r2 r3)))
	     (cond
	      ((not (equal? after1 after2))
	       (format #t "disagreement~%")
	       (format #t "inputs (~a ~a ~a ~a ~a ~a ~a ~a ~a) ~%" title1 op a b c r0 r1 r2 r3)
	       (format #t "inputs (~a ~a ~a ~a ~a ~a ~a ~a ~a) ~%" title2 op a b c r0 r1 r2 r3)	       
	       (format #t "after1 = ~a ~%" after1)
	       (format #t "after2 = ~a ~%" after2)
	       (error "check2")))))
	  (_ (error "no match")))))))

(define (check3)
  (map (lambda (x)
	 (begin (map (apply check2 x) *samples*) #t))
       (list
	(list f/addr "f/addr" g/addr "g/addr")
	(list f/addi "f/addi" g/addi "g/addi")

	(list f/mulr "f/mulr" g/mulr "g/mulr")
	(list f/muli "f/muli" g/muli "g/muli")
	
	(list f/banr "f/banr" g/banr "g/banr")
	(list f/bani "f/bani" g/bani "g/bani")
	
	(list f/borr "f/borr" g/borr "g/borr")
	(list f/bori "f/bori" g/bori "g/bori")
	
	(list f/setr "f/setr" g/setr "g/setr")
	(list f/seti "f/seti" g/seti "g/seti")

	(list f/gtir "f/gtir" g/gtir "g/gtir")

	)))




#|

        < cmd > < in-->
(f/addr 0 3 3 2 3 0 0 2)
(g/addr 0 3 3 2 3 0 0 2)

(3 0 4 2)




|#






	  
(define part-a
  (lambda ()
    (length
     (filter (lambda (x) (>= x 3)) (demo2)))))

#|

$20 = (4 9 9 3 4 10 3 6 3 13 7 3 9 7 8 8 7 8 7 6 9 8 3 9 3 4 8 3 4 8 3 13 3 3 8 8 3 13 13 13 8 3 8 4 9 6 3 7 13 6 8 6 8 3 7 8 8 10 8 8 6 6 8 3 3 6 7 8 8 3 7 8 10 4 8 13 9 9 7 9 6 3 13 6 3 4 6 3 7 4 8 8 3 3 13 9 8 8 9 7 7 8 3 3 7 3 3 3 4 3 8 6 3 9 8 8 3 9 8 9 6 3 6 8 8 3 8 13 8 7 8 10 8 3 8 7 4 6 6 8 8 13 3 8 3 13 9 7 3 3 10 8 7 3 3 13 3 8 8 7 3 8 3 8 8 6 3 7 3 3 8 13 7 8 10 3 6 6 9 7 6 4 13 8 4 10 8 10 8 7 8 9 3 8 3 8 13 3 3 3 8 3 3 6 3 3 4 9 8 3 10 8 8 10 6 8 6 8 8 3 6 8 4 3 6 3 13 3 3 6 13 8 10 8 8 9 4 9 10 4 8 6 4 8 9 8 3 8 4 8 8 8 8 7 8 4 9 13 8 8 4 13 4 7 8 8 9 4 10 3 8 13 8 9 6 8 4 3 3 3 13 6 7 13 3 8 6 8 3 3 6 4 10 10 3 8 7 3 4 4 8 9 3 13 4 7 3 9 8 3 3 10 13 10 7 10 3 3 10 3 3 8 10 7 9 3 8 6 8 6 3 3 10 10 3 7 8 9 8 8 10 10 8 3 8 3 8 4 3 9 9 6 8 4 3 13 3 3 10 7 3 8 4 7 13 8 7 8 8 7 7 7 8 9 8 3 3 6 3 13 6 13 9 7 10 8 8 10 13 8 8 13 8 9 4 3 4 6 8 13 8 7 6 10 13 10 3 8 3 4 13 9 13 3 3 9 3 8 7 3 8 8 3 7 7 7 7 8 13 10 3 8 8 8 3 13 10 10 3 3 6 13 8 6 3 10 13 6 8 8 3 7 8 6 3 3 13 4 3 6 10 8 3 3 9 13 3 8 3 4 4 8 6 3 8 8 7 10 8 8 8 3 10 8 8 3 3 7 8 13 6 8 8 8 3 8 3 3 8 3 3 10 13 3 3 10 8 13 3 3 8 3 8 3 7 6 3 8 8 8 6 8 4 3 4 3 9 8 10 8 8 4 8 3 6 13 3 3 13 3 10 8 10 9 3 10 6 10 3 8 8 8 9 3 6 3 3 7 8 10 10 8 7 9 3 8 6 13 6 9 8 8 7 6 3 6 6 3 3 8 13 6 9 4 10 6 8 8 7 10 10 3 6 7 4 8 3 8 13 4 7 8 3 10 8)

scheme@(guile-user) [2]> (length (filter (lambda (x) (>= x 3)) (demo)))
 605



|#










