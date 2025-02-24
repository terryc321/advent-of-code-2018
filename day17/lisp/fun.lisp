;;;; fun.lisp

(in-package #:aoc18)

;;(ppcre:all-matches-as-strings "\\d+" "numbers: 1 10 42")
(defun read-from-file (filename)
  (labels ((parse-if-string (x)
	     (cond
	       ((stringp x) (parse-integer x))
	       (t x))))
  (let ((xs nil)
	(in (open filename :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do 
            (format t "~a~%" line)
	    (let* ((has-digits (ppcre:all-matches-as-strings "\\d+" line))
		   (int-values (or
				(ppcre:register-groups-bind (x1 y1 y2)
				    ("x=(\\d+).*y=(\\d+)\.\.(\\d+)" line :sharedp t)
				  (mapcar #'parse-if-string (list 'x-y-y x1 y1 y2)))
				(ppcre:register-groups-bind (x1 y1 y2)
				    ("y=(\\d+).*x=(\\d+)\.\.(\\d+)" line :sharedp t)
				  (mapcar #'parse-if-string (list 'y-x-x x1 y1 y2))))))
	      (cond
		((and has-digits (not int-values)) (error (format nil "bad read on ~a" line)))
		(int-values 
		 (setq xs (cons int-values xs)))))))
    (setq xs (reverse xs))
    xs)))


;; read lines from
(defparameter input (read-from-file "../input.txt"))
(defparameter example (read-from-file "../example.txt"))

;; given data figure out max and min values of x , y
;; create a grid and show result
;; top left is lowest x , y
;; bottom right is highest x ,y

;; s1 < s2 then set s2
;; s1 > s3 then set s3
(defmacro %min-max (s1 s2 s3)  
  `(progn
     (when (or (not ,s2) (< ,s1 ,s2)) (setq ,s2 ,s1))
     (when (or (not ,s3) (> ,s1 ,s3)) (setq ,s3 ,s1))))

(let ((x 5)(y nil)(y2 nil))
  (min-max x y y2)
  (list 'x= x 'y= y 'y2= y2))

(let ((x 5)(y nil)(y2 nil))
  (min-max 5 y y2)
  (min-max 2 y y2)
  (list 'x= x 'y= y 'y2= y2))

(defmacro %min (s1 s2)  
  `(progn
     (when (or (not ,s2) (< ,s1 ,s2)) (setq ,s2 ,s1))))

(defmacro %max (s1 s2)  
  `(progn
     (when (or (not ,s2) (> ,s1 ,s2)) (setq ,s2 ,s1))))


(defstruct grid
  (width 0)
  (height 0)
  (max-x 0)
  (min-x 0)
  (max-y 0)
  (min-y 0)
  (data nil))



(defun process (xs)
  (let ((min-x nil)(min-y nil)(max-x nil)(max-y nil)(grid nil)(off-x nil)(off-y nil))
    (labels ((process-x-y-y (x y1 y2)
	       (%min x min-x)
	       (%max x max-x)
	       (%min y1 min-y)
	       (%max y2 max-y))
	     (process-y-x-x (y x1 x2)
	       (%min y min-y)
	       (%max y max-y)
	       (%min x1 min-x)
	       (%max x2 max-x))
	     (fill-x-y-y (x y1 y2)
	       (loop for y from y1 to y2 do
		 (setf (aref grid (- x off-x) (- y off-y)) t)))
	     (fill-y-x-x (y x1 x2)
	       (loop for x from x1 to x2 do
		 (setf (aref grid (- x off-x) (- y off-y)) t)))
	     (process-grid (xs)
	       (cond
		 ((null xs) xs)
		 (t
		  (destructuring-bind (type n1 n2 n3) (car xs)
		    (cond
		      ((eq type 'x-y-y) (fill-x-y-y n1 n2 n3))
		      ((eq type 'y-x-x) (fill-y-x-x n1 n2 n3))
		      (t (error "bad-type ")))
		    (process-grid (cdr xs))))))
	     (process-helper (xs)
	       (cond
		 ((null xs) xs)
		 (t
		  (destructuring-bind (type n1 n2 n3) (car xs)
		    (cond
		      ((eq type 'x-y-y) (process-x-y-y n1 n2 n3))
		      ((eq type 'y-x-x) (process-y-x-x n1 n2 n3))
		      (t (error "bad-type ")))
		    (process-helper (cdr xs)))))))
      (process-helper xs)
      (let ((width (+ 2 (- max-x min-x)))
	    (height (+ 2 (- max-y min-y))))	   
	(let ((tmp (make-array (list width height) :initial-element nil)))
	  (setq off-x min-x)
	  (setq off-y min-y)
	  (setq grid tmp)
	  (process-grid xs)	  
	  (format t "~a~%"
		  (list 'grid-is min-x min-y 'to max-x max-y))
	  (let ((result (make-grid :width width :height height :data grid
				   :min-x min-x :max-x max-x :min-y min-y
				   :max-y max-y)))
	    result))))))

(defun show-grid (g)
  (let ((width (grid-width g))
	(height (grid-height g))
	(data (grid-data g))
	(min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    (format t "~%")
    (loop for y from 0 to (- height 1) do
      (loop for x from 0 to (- width 1) do
	(let ((val (aref data x y)))
	  (cond
	    (val (format t "#" ))
	    (t (format t "." )))))
      (format t "~%"))))

      



