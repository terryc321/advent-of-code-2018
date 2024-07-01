
(defpackage :foo
	    (:use :cl))
(in-package :foo)

(format t "hello world~%")


#|

cmd command
OP A B C

input (four register values)
r0 r1 r2 r3

(ra) macro expand in context of
(defun addr (cmd in) ...
        (ra)
        
(addr '(0 0 1 2) '(0 1 2 3))    
(addi '(0 0 1 2) '(2 2 3 0))

(let ((in '(123 456 789 101112)))
  (store 0 1))
(let ((in '(123 456 789 101112)))
  (store 1 1))
(let ((in '(123 456 789 101112)))
  (store 2 1))
(let ((in '(123 456 789 101112)))
  (store 3 1))

Equality testing:

    eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
    eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
    eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
|#



;; value of register a 
(defmacro va ()
  `(nth 1 cmd))

(defmacro vb ()
  `(nth 2 cmd))

(defmacro vc ()
  `(nth 3 cmd))


;; 
(defmacro ra ()
  `(nth (nth 1 cmd) in))

(defmacro rb ()
  `(nth (nth 2 cmd) in))

(defmacro rc ()
  `(nth (nth 3 cmd) in))

(defmacro rop ()
  `(nth 0 cmd))

(defmacro store (i x)
  `(replace-nth ,i ,x in))

(defun replace-nth (i x in)
  (cond
    ((= i 0)
     (cond
       ((null in) (cons x nil))
       (t (cons x (cdr in)))))
    (t (cons (car in) (replace-nth (- i 1) x (cdr in))))))

(replace-nth 0 1 '(5 6 7 8))
(replace-nth 1 1 '(5 6 7 8))
(replace-nth 2 1 '(5 6 7 8))
(replace-nth 3 1 '(5 6 7 8))


;;  C could be 0 1 2 3
;;  A could be 0 1 2 3
;;  B could be 0 1 2 3

;;   addr (add register) stores into register C the result of adding register A and register B.
(defun addr (cmd in)  (let* ((x (ra))	 (y (rb)))    (store (rc) (+ x y))))

;;  addi (add immediate) stores into register C the result of adding register A and value B.
(defun addi (cmd in) (let* ((x (ra)) (y (vb)))  (store (rc) (+ x y))))

;; mulr (multiply register) stores into register C the result of multiplying register A and register B.
(defun mulr (cmd in)  (let* ((x (ra))	 (y (rb)))    (store (rc) (* x y))))

;; muli (multiply immediate) stores into register C the result of multiplying register A and value B.
(defun muli (cmd in)  (let* ((x (ra))	 (y (vb)))    (store (rc) (* x y))))


;; banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
(defun banr (cmd in)  (let* ((x (ra))	 (y (rb)))    (store (rc) (logand x y))))

;; bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
(defun bani (cmd in)  (let* ((x (ra))	 (y (vb)))    (store (rc) (logand x y))))


;; borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.                                    
(defun borr (cmd in)  (let* ((x (ra))	 (y (rb)))    (store (rc) (logior x y))))
;; bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
(defun bori (cmd in)  (let* ((x (ra))	 (y (vb)))    (store (rc) (logior x y))))


;; setr (set register) copies the contents of register A into register C. (Input B is ignored.)
(defun setr (cmd in)  (let* ((x (ra)))   (store (rc) x)))

;; seti (set immediate) stores value A into register C. (Input B is ignored.)
(defun seti (cmd in)  (let* ((x (va)))    (store (rc) x)))

;; gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
(defun gtir (cmd in)  (let* ((x (va)) (y (rb)))   (store (rc) (if (> x y) 1 0))))

;; gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
(defun gtri (cmd in)  (let* ((x (ra)) (y (vb)))   (store (rc) (if (> x y) 1 0))))

;; gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
(defun gtrr (cmd in)  (let* ((x (ra)) (y (rb)))   (store (rc) (if (> x y) 1 0))))

;; eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
(defun eqir (cmd in)  (let* ((x (va)) (y (rb)))   (store (rc) (if (= x y) 1 0))))

;; eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
(defun eqri (cmd in)  (let* ((x (ra)) (y (vb)))   (store (rc) (if (= x y) 1 0))))

;; eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
(defun eqrr (cmd in)  (let* ((x (ra)) (y (rb)))   (store (rc) (if (= x y) 1 0))))



(defparameter *samples*
  (let ((xs '()))	
    (with-open-file (in "input-a.lisp")
      (catch 'done
	(loop while t do
	  (let ((expr (read in nil nil)))
	    (cond
	      ((null expr) (throw 'done t))
	      (t (setq xs (cons expr xs))))))))
    (setq xs (reverse xs))
    xs))


(defun process-samples ()
  (let ((three-count 0))
    (dolist (sample *samples*)
      (let ((before (first sample))
	    (cmd (second sample))
	    (after (third sample)))
	(let ((len (length 
		    (remove-if-not
		     (lambda (x) (equalp x after))
		     (mapcar (lambda (fn) (funcall fn cmd before))
			     (list #'addr #'addi
				   #'mulr #'muli
				   #'banr #'bani
				   #'borr #'bori
				   #'setr #'seti
				   #'gtir #'gtri
				   #'gtrr #'eqir 
				   #'eqri #'eqrr
				   ))))))
	  (format t "~a : len = ~a ~%"  sample len)
	  (cond
	    ((>= len 3) (incf three-count))))))
    three-count))




#|

nope ... ha ha ....

345 (9 bits, #x159)  ....... REJECTED .......



|#








	    
	    





