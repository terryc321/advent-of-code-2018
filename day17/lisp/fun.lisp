;;;; fun.lisp

(in-package #:aoc18)

;; s1 < s2 then set s2
;; s1 > s3 then set s3
(defmacro min-max! (s1 s2 s3)  
  `(progn
     (when (or (not ,s2) (< ,s1 ,s2)) (setq ,s2 ,s1))
     (when (or (not ,s3) (> ,s1 ,s3)) (setq ,s3 ,s1))))

(let ((x 5)(y nil)(y2 nil))
  (min-max! x y y2)
  (list 'x= x 'y= y 'y2= y2))

(let ((x 5)(y nil)(y2 nil))
  (min-max! 5 y y2)
  (min-max! 2 y y2)
  (list 'x= x 'y= y 'y2= y2))

(defmacro min! (s1 s2)  
  `(progn
     (when (or (not ,s2) (< ,s1 ,s2)) (setq ,s2 ,s1))))

(defmacro max! (s1 s2)  
  `(progn
     (when (or (not ,s2) (> ,s1 ,s2)) (setq ,s2 ,s1))))


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
	       (min! x min-x)
	       (max! x max-x)
	       (min! y1 min-y)
	       (max! y2 max-y))
	     (process-y-x-x (y x1 x2)
	       (min! y min-y)
	       (max! y max-y)
	       (min! x1 min-x)
	       (max! x2 max-x))
	     (fill-x-y-y (x y1 y2)
	       (loop for y from y1 to y2 do
		 (setf (aref grid (- x off-x) (- y off-y)) #\#)))
	     (fill-y-x-x (y x1 x2)
	       (loop for x from x1 to x2 do
		 (setf (aref grid (- x off-x) (- y off-y)) #\#)))
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
	(let ((tmp (make-array (list width height) :initial-element #\.)))
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
	(let ((px (+ x min-x))
	      (py (+ y min-y)))
	  (let ((val (aref data x y)))
	    (cond
	      ((and (= px 500)(= py 1) (equalp val #\#)) (error "something at sprinker!"))
	      ((and (= px 500)(= py 1)) (format t "+"))
	      ((equalp val #\#) (format t "#" ))
	      ((equalp val #\.) (format t "." ))
	      ((equalp val #\o) (format t "o" ))
	      ((equalp val #\~) (format t "~a" #\~))	      
	      ((equalp val #\?) (format t "?" ))	      
	      (t (format t "/" ))))))
      (format t "~%"))))



;; fill routine comes out sprinkler to come down
;; it stops if it hits clay , move left right until nothing underneath it
;; ;;
;; #..#.xxx...#.
;; #..#.x#x.....
;; #..#.x#......
;; #...xx#x.....
;; #xxxxx#......
;; #######x.....
;; .............
;; .......x.....
;;

(defun on-grid (g x y)
  (let ((width (grid-width g))
	(height (grid-height g))
	(min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    ;; (and (>= x min-x) (<= x min-y)
    ;; 	 (>= y min-y) (<= y max-y))
    (and (>= x 0) (<= x width)
	 (>= y 0) (<= y height))))

(defun at (g x y)
  (cond
    ((on-grid g x y) (aref (grid-data g) x y))
    (t nil)))

;; ;; is there anything underneath the sprinkler
;; ......x......
;; ......x......
;; ...#..x..#...
;; ...#..x..#...
;; ...#xx?xx#...
;; ...#######...
;; .............
;; when gets to bottom and hits clay asks , is it boxed in by clay
;; 

(defun drip-recur (g x y)
  (let ((width (grid-width g))
	(height (grid-height g))
	(data (grid-data g))
	(min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    (let ((val (at g x y)))
      (cond
	((not (on-grid g x y)) nil)
	((equalp val #\#) nil)
	((equalp val #\.)
	 (let ((val2 (at g x (+ y 1))))
	   (cond
	     ((equalp val2 #\#) ;; clay below us		
	      (setf (aref data x y) #\~)
	      (drip-recur g (- x 1) y)
	      (drip-recur g (+ x 1) y))
	     ((equalp val2 #\~) ;; water below us		
	      ;;(setf (aref data x y) #\~)
	      nil
	      )
	     ((equalp val2 #\.)
	      ;;(setf (aref data x y) #\o)
	      (fill-recur g x (+ y 1))
	      ))))))))





(defun fill-recur (g x y)
  (let ((width (grid-width g))
	(height (grid-height g))
	(data (grid-data g))
	(min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    (let ((val (at g x y)))
      (cond
	((not (on-grid g x y)) nil)
	((equalp val #\#) nil)
	((equalp val #\.)
	 (let ((val2 (at g x (+ y 1))))
	   (cond
	     ((equalp val2 #\#) ;; clay below us		
	      (setf (aref data x y) #\~)
	      (drip-recur g (- x 1) y)
	      (drip-recur g (+ x 1) y))
	     ((equalp val2 #\.)
	      ;;(setf (aref data x y) #\o)
	      (fill-recur g x (+ y 1))
	      (when (and (equalp #\~ (at g x (+ y 1)))
			 (find-clay-left g x y)
			 (find-clay-right g x y))
		(format t "overflow !~%")
		(setf (aref data x y) #\~)		
		(drip-recur g (- x 1) y)
		(drip-recur g (+ x 1) y)))))
	 (setf (aref data x y) #\~)
	 )))))


(defun find-clay-left (g x y)
  (cond
    ((equalp (at g x y) #\.) (find-clay-left g (- x 1) y))
    ((equalp (at g x y) #\#) t)
    (t nil)))

(defun find-clay-right (g x y)
  (cond
    ((equalp (at g x y) #\.) (find-clay-right g (+ x 1) y))
    ((equalp (at g x y) #\#) t)
    (t nil)))

(defun water-left (g x y)  (equalp (at g (- x 1) y) #\~))
(defun water-right (g x y)  (equalp (at g (+ x 1) y) #\~))


;; assume sprinkler is top of data ??
(defun sprinkler (g)
  (let ((width (grid-width g))
	(height (grid-height g))
	(data (grid-data g))
	(min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    (let ((sprinkler-x (- 500 min-x))
	  (sprinkler-y (- 1 min-y)))      
      (fill-recur g sprinkler-x sprinkler-y))))


(defparameter g (process example))

(defun run ()
  (show-grid g)
  (sprinkler g)
  (show-grid g))




;;(trace drip-recur)
;;(untrace)










      



