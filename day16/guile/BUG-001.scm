

#|


BUG 001 ......

(f/addr 0 3 3 2 3 0 0 2) 
$51 = (3 0 4 2)

(g/addr 0 3 3 2 3 0 0 2) 
$52 = (3 0 2 2)

expected that f/addr and g/addr give same result 


|#

(use-modules (srfi srfi-1))
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 match))

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
		 ((= b 3) (fn2 r2 b))
		 (#t (error (format #f "~a no b" str)))))
	     (out (fn x y)))
	(cond
	 ((= c 0) (list out r1 r2 r3))
	 ((= c 1) (list r0 out r2 r3))
	 ((= c 2) (list r0 r1 out r3))
	 ((= c 3) (list r0 r1 r2 out ))
	 (#t (error (format #f "~a no c" str))))))))


(define g/addr (g/reg-or-imm + (lambda (x y) x) "g/addr"))

;; substitute + for fn
;; substitute 1st arg for fn2 call
(define g/addr2
  (let ((str "g/addr2")
	(fn +)
	(fn2 (lambda (x y) x)))
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
		 ((= b 3) (fn2 r2 b))
		 (#t (error (format #f "~a no b" str)))))
	     (out (fn x y)))
	(cond
	 ((= c 0) (list out r1 r2 r3))
	 ((= c 1) (list r0 out r2 r3))
	 ((= c 2) (list r0 r1 out r3))
	 ((= c 3) (list r0 r1 r2 out ))
	 (#t (error (format #f "~a no c" str))))))))


;; literally rewrite fn to be +
;; literally rewrite fn2 to second arg passed to fn2
(define g/addr3
  (let ((str "g/addr2")
	(fn +)
	(fn2 (lambda (x y) x)))
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
		 ((= b 3) (fn2 r2 b)) <<<< bug is here :: should be r3
		 (#t (error (format #f "~a no b" str)))))
	     (out (fn x y)))
	(cond
	 ((= c 0) (list out r1 r2 r3))
	 ((= c 1) (list r0 out r2 r3))
	 ((= c 2) (list r0 r1 out r3))
	 ((= c 3) (list r0 r1 r2 out ))
	 (#t (error (format #f "~a no c" str))))))))


