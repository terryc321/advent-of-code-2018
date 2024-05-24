
(defpackage :foo
  (:use :cl))

(in-package :foo)

(declaim (optimize (speed 0)(space 0)(safety 3)(debug 3)(compilation-speed 0)))

;;(declaim (optimize (speed 3)(space 0)(safety 0)(debug 0)(compilation-speed 0)))

#|

Your puzzle input is 768071

two elfs start with 3 and 7

sum together gives 1 0
first score 1  ,second score 0 

list access slow , move to hash table ?

|#

(defun char-to-int (ch)
  (cond
    ((char= ch #\0) 0)
    ((char= ch #\1) 1)
    ((char= ch #\2) 2)
    ((char= ch #\3) 3)
    ((char= ch #\4) 4)
    ((char= ch #\5) 5)
    ((char= ch #\6) 6)
    ((char= ch #\7) 7)
    ((char= ch #\8) 8)
    ((char= ch #\9) 9)
    (t (error "char-to-int"))))


(defun an (n)
  (declare (type fixnum n))
  (char-to-int (char (format nil "~a" n) 0)))

(defun bn (n)
  (declare (type fixnum n))
  (cond
    ((< n 10) nil)
    (t (char-to-int (char (format nil "~a" n) 1)))))

#|

list based version 

|#
(defun f1 ()
  (let ((xs '(3 7))
	(a 0)
	(b 1)
	(step 1)
	(len 2)
	)
    ;; step 1 
    ;;(fshow step xs a b)
    
    (loop
      ;;while t
      for j from 1 to 10000000
      do 
	 (incf step)
	 ;;(format t "step ~a ~%" step)
	 ;;(format t "step ~a : xs = ~a ~%" step xs)

	 (when (zerop (mod step 10000))
	   (format t "step ~a ~%" step))
	 
	 
	 (let* ((ai (nth a xs))
		(bi (nth b xs))
		(sum (+ ai bi))
		(a2 (an sum))
		(b2 (bn sum)))
	   (cond
	     ((< sum 10) ; only one number
	      ;;(format t "assign ~a -> ~a ~%" a2 len)
	      (setq len (+ len 1))
	      (setq xs (append xs (list a2)))
	      t)
	     (t
	      ;;(format t "assign ~a -> ~a ~%" a2 len)
	      ;;(format t "assign ~a -> ~a ~%" b2 (+ len 1))

	      (setq len (+ len 2))
	      (setq xs (append xs (list a2 b2)))))
	   
	   ;;(format t "a = ~a : b = ~a : sum = ~a : a2 = ~a : b2 = ~a ~%" a b sum a2 b2)

	   ;; advance a by ai + 1
	   (loop for i from 1 to (+ ai 1) do
	     (setq a (+ a 1))
	     (when (>= a len)
	       (setq a 0)))
	   
	   ;; advance b by bi + 1 
	   (loop for i from 1 to (+ bi 1) do
	     (setq b (+ b 1))
	     (when (>= b len)
	       (setq b 0)))

	   ;;(fshow step xs a b)

	   ;; (when
	   ;;     (or (> step 768070 ))
	   ;; 	(fshow step xs a b)
	   ;; 	)

	   
	   ))))



(defun fshow (step xs a b)
  (let ((i 0))
    (format t "step ~a : ( " step)
    (dolist (x xs)
      (let ((s1 0)(s2 0)(s3 0)(sall 0))
	(when (= a i) (format t "(~a) " x) (incf s1))
	(when (= b i) (format t "[~a] " x) (incf s2))
	(when (and (not (= a i))
		  (not (= b i)))
	  (format t "~a " x)
	  (incf s3)
	  )
	(setq sall (+ s1 s2 s3))
	(when (> sall 1)
	  (error "fshow something not right"))	    
	(incf i)))
    (format t ")~%")))



#|

hash table version 
eql numeric test


|#
(defun f2 ()
  (let ((xs (let ((h (make-hash-table :test #'eql)))
	      (setf (gethash 0 h) 3)
	      (setf (gethash 1 h) 7)
	      h))
	(a 0)
	(b 1)
	(step 1)
	(len 2)
	(end 1)
	(recipe5 nil)
	(recipe18 nil)
	(recipe2018 nil)
	(recipe768071 nil)
	)
    
    ;; step 1 
    ;;(fshow step xs a b)
      
    (loop
      ;;while t
      ;;for j from 1 to 20
      for j from 1 to 1000000
      ;;for j from 1 to 10000
      do 
	 (incf step)
	 
	 ;; (when (zerop (mod step 10000))
	 ;;   (format t "step ~a ~%" step))
	 
	 
		     ;;(format t "step ~a ~%" step)
      ;;(format t "step ~a : xs = ~a ~%" step xs)

      (let* ((ai (gethash a xs))
	     (bi (gethash b xs))
	     (sum (+ ai bi))
	     (a2 (an sum))
	     (b2 (bn sum)))
	(cond
	  ((< sum 10) ; only one number
	   (incf end)
	   ;;(format t "assign ~a -> ~a ~%" a2 end)
	   (setf (gethash end xs) a2)	   
	   (setq len (+ len 1))
	   ;;(setq xs (append xs (list a2)))
	   t)
	  (t
	   ;; to numbers made
	   (incf end)
	   ;;(format t "assign ~a -> ~a ~%" a2 end)
	   (setf (gethash end xs) a2)
	   (incf end)
	   ;;(format t "assign ~a -> ~a ~%" b2 end)	   
	   (setf (gethash end xs) b2)
	   (setq len (+ len 2))
	   ;;(setq xs (append xs (list a2 b2)))
	   ))
	
	;;(format t "a = ~a : b = ~a : sum = ~a : a2 = ~a : b2 = ~a ~%" a b sum a2 b2)

	;; advance a by ai + 1
	(loop for i from 1 to (+ ai 1) do
	  (setq a (+ a 1))
	  (when (>= a len)
	    (setq a 0)))
	
	;; advance b by bi + 1 
	(loop for i from 1 to (+ bi 1) do
	  (setq b (+ b 1))
	  (when (>= b len)
	    (setq b 0)))

	;;(fshow2 step len xs a b)	
	;;(fshow2-last10 step len xs a b)

	(when (and (not recipe5) (> len (+ 5 10)))
	    (fshow2-last10-after step 5 xs a b)
	    (setq recipe5 t))

	(when (and (not recipe18) (> len (+ 18 10)))
	    (fshow2-last10-after step 18 xs a b)
	    (setq recipe18 t))

	(when (and (not recipe2018) (> len (+ 2018 10)))
	    (fshow2-last10-after step 2018 xs a b)
	    (setq recipe2018 t))

	;;768071
 	(when (and (not recipe768071) (> len (+ 768071 10)))
	    (fshow2-last10-after step 768071 xs a b)
	    (setq recipe768071 t))
	
	;; (when
	;;     (or (> step 768070 ))
	;; 	(fshow step xs a b)
	;; 	)
	
	))))



(defun fshow2 (step len xs a b)
    (format t "[fshow2] step ~a : ( " step)
    (loop for i from 0 to (- len 1) do 
      (let ((s1 0)(s2 0)(s3 0)(sall 0)(x (gethash i xs)))
	(when (= a i) (format t "(~a) " x) (incf s1))
	(when (= b i) (format t "[~a] " x) (incf s2))
	(when (and (not (= a i)) (not (= b i)))
	  (format t "~a " x)
	  (incf s3))
	(setq sall (+ s1 s2 s3))
	(when (> sall 1)
	  (error "fshow something not right"))	    
	;;(incf i)
	))
  (format t ")~%"))



(defun fshow2-last10-after (step after xs a b)
    (format t "[fshow2-last10] step ~a : ( " step)
    (loop for i from 1 to 10 do 
      (let ((x (gethash (+ -1 after i) xs)))
	(format t "~a " x)
	))
  (format t ")~%"))


#|

(3)[7]
(3)[7] 1  0 
 3  7  1 [0](1) 0 
 3  7  1  0 [1] 0 (1)
(3) 7  1  0  1  0 [1] 2 
 3  7  1  0 (1) 0  1  2 [4]
 3  7  1 [0] 1  0 (1) 2  4  5 
 3  7  1  0 [1] 0  1  2 (4) 5  1 
 3 (7) 1  0  1  0 [1] 2  4  5  1  5 
 3  7  1  0  1  0  1  2 [4](5) 1  5  8 
 3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
 3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6 
 3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7 
 3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7 
 3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9 
 3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2 


next ten scores after 5 recipes means need to make a seuqence of atleast 15 in length 

After 5 recipes, the scores of the next ten would be 0124515891.
 ^  ^  ^  ^  ^  ******************************************
                0  1  2  4  5  1  5  8  9  1   



After 18 recipes, the scores of the next ten would be 9251071085.
> need sequence step of atleast 

After 2018 recipes, the scores of the next ten would be 5941429882.

|#



#|

FOO> (f2)
[fshow2-last10] step 12 : ( 0 1 2 4 5 1 5 8 9 1 )
[fshow2-last10] step 23 : ( 9 2 5 1 0 7 1 0 8 5 )
[fshow2-last10] step 1519 : ( 5 9 4 1 4 2 9 8 8 2 )
[fshow2-last10] step 590980 : ( 6 5 4 8 1 0 3 9 1 0 )
NIL

6548103910  ...... ACCEPTED answer ! 



|#






