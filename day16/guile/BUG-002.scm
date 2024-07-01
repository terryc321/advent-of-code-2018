

#|

(f/borr 0 3 3 0 2 3 2 2) 
 = (2 3 2 2)
 (g/borr 0 3 3 0 2 3 2 2) 
 = (3 3 2 2)

expected same



|#

(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))


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



;; ---- a more generic version handle both register and immediate 
(define g/reg-or-imm
  (lambda (fn fn2 str)
    (lambda (op a b c r0 r1 r2 r3)
      (let* ((x (cond
		 ((= a 0) r0)
		 ((= a 1) r1)
		 ((= a 2) r2)
		 ((= a 3) r3)
		 (#t (error (format #f "~a no a" str)))))
	     (y (cond ;; value of b
		 ((= b 0) (fn2 r0 b))
		 ((= b 1) (fn2 r1 b))
		 ((= b 2) (fn2 r2 b))
		 ((= b 3) (fn2 r3 b))
		 (#t (error (format #f "~a no b" str)))))
	     (out (fn x y)))
	(cond
	 ((= c 0) (list out r1 r2 r3))
	 ((= c 1) (list r0 out r2 r3))
	 ((= c 2) (list r0 r1 out r3))
	 ((= c 3) (list r0 r1 r2 out ))
	 (#t (error (format #f "~a no c" str))))))))

                                                 ***
(define g/borr (g/reg-or-imm logior (lambda (x y) y) "g/borr"))
                                                 ^^^^ shoudlbe be x 

