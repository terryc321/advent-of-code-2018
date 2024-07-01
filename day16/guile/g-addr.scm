
;; aoc 2018 day16 

(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))

(define pp pretty-print)


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

(define h/remove
  (lambda (x ys)
    (cond
     ((null? ys) '())
     ((equal? x (car ys)) (h/remove x (cdr ys)))
     (#t (cons (car ys) (h/remove x (cdr ys)))))))

  

;; find opcode of h/addr - find values it cannot be then left with choice
(define h/find
  (lambda (fn str)
    (let* (
	   (opcodes '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	   (fsample     (lambda (sample)
			  (let ((before (first sample))
				(after (third sample))
				(command (second sample)))
			    (match (append command before)
			      ((op a b c r0 r1 r2 r3)
			       (let ((out (fn op a b c r0 r1 r2 r3)))
				 (cond
				  ((equal? out after) #f)
				  (#t (set! opcodes (h/remove op opcodes))))))
			      (_ (error "no match")))))))
      (map fsample *samples*)
      (list (string->symbol str) opcodes))))


(define demo
  (lambda ()
    (letrec ((groups '())
	     (add (lambda (x)
		    (set! groups (cons x groups)))))
      (add (h/find g/addr "g/addr"))
      (add (h/find g/addi "g/addi"))
      (add (h/find g/mulr "g/mulr"))
      (add (h/find g/muli "g/muli"))
      (add (h/find g/banr "g/banr"))
      (add (h/find g/bani "g/bani"))
      (add (h/find g/borr "g/borr"))
      (add (h/find g/bori "g/bori"))
      (add (h/find g/setr "g/setr"))
      (add (h/find g/seti "g/seti"))
      (add (h/find g/gtir "g/gtir"))
      (add (h/find g/gtri "g/gtri"))
      (add (h/find g/gtrr "g/gtrr"))
      (add (h/find g/eqir "g/eqir"))
      (add (h/find g/eqri "g/eqri"))
      (add (h/find g/eqrr "g/eqrr"))
      groups
      ;; find a group with length of 1 -- fully constrained
      ;; label it
      ;; remove it from groups
      ;; keep going until groups empty
      (d/fix groups)
      )))

(define d/fix  
  (lambda (groups)
    (let ((singles (filter (lambda (x) (= (length (second x)) 1)) groups)))
      (cond
       ((null? singles) #f)
       (#t (let* ((single (first singles))
		  (sym (first single))
		  (val (first (second single))))
	     (format #t " ~a ==> ~a ~%" sym val)
	     (let ((new-groups (filter (lambda (s) (not (equal? s single))) groups)))
	       ;;(format #t "new-groups ~%")
	       ;;(pp new-groups)
	       (let ((reduced-groups (map (lambda (pair)
					    (let ((sym (first pair))
						  (vals (filter (lambda (v) (not (= v val))) (second pair))))
					      (list sym vals)))
					  new-groups)))
		 ;; d/fix
		 (d/fix reduced-groups)))))))))

#|

generate this table of results

scheme@(guile-user)> (pp (demo))
 g/muli ==> 8 
 g/bori ==> 9 
 g/borr ==> 13 
 g/setr ==> 10 
 g/addr ==> 11 
 g/addi ==> 5 
 g/mulr ==> 1 
 g/seti ==> 6 
 g/bani ==> 12 
 g/banr ==> 4 
 g/gtir ==> 7 
 g/eqrr ==> 15 
 g/eqri ==> 0 
 g/eqir ==> 14 
 g/gtri ==> 2 
 g/gtrr ==> 3 


|#

