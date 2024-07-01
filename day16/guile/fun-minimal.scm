
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
  (let ((fn-list (list
		  g/addr g/addi g/mulr g/muli g/banr g/bani g/borr
		  g/bori g/setr g/seti g/gtir g/gtri g/gtrr g/eqir
		  g/eqri g/eqrr)))
    (lambda (sample)
      (let ((before (first sample))
	    (after (third sample))
	    (command (second sample)))
	(match (append command before)
	  ((op a b c r0 r1 r2 r3)
	   (length
	    (filter
	     (lambda (x) (equal? x after))
	     (map (lambda (fn)
		    (fn op a b c r0 r1 r2 r3))
		  fn-list))))
	  (_ (error "no match")))))))


(define part-a
  (lambda ()
    (let ((table (map process *samples*)))
      (length (filter (lambda (x) (>= x 3)) table)))))


