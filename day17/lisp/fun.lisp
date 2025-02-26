;;;; fun.lisp

(in-package #:aoc18)

(declaim (optimize (debug 3)))

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
		 (setf (gethash (list x y) grid) #\#)))	     
	;;	 (setf (aref grid (- x off-x) (- y off-y)) #\#)))
	     (fill-y-x-x (y x1 x2)
	       (loop for x from x1 to x2 do
		 (setf (gethash (list x y) grid) #\#)))	     
		 ;;(setf (aref grid (- x off-x) (- y off-y)) #\#)))
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
      (let ((tmp (make-hash-table :test #'equalp))) ;;(make-array (list width height) :initial-element #\.)))
	(setq off-x min-x)
	(setq off-y min-y)
	(setq grid tmp)
	(loop for y from min-y to max-y do
	  (loop for x from min-x to max-x do
	    (setf (gethash (list x y) grid) #\.))) ;; all sand initially
	(process-grid xs)	  
	(format t "~a~%"
		(list 'grid-is min-x min-y 'to max-x max-y))
	(let ((result (make-grid :data grid
				 :min-x min-x :max-x max-x :min-y min-y
				 :max-y max-y)))
	  result)))))




(defun show-grid (g)
  (let ((data (grid-data g))
	(min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    (format t "~%")
    ;; (loop for y from (- min-y 3) to (+ max-y 3) do
     (loop for y from 0 to (+ max-y 3) do    
      (loop for x from (- min-x 3) to (+ max-x 3) do 
	(let ((val (gethash (list x y) data)))
	  (cond
	    ((and (= x 500)(= y 0) (equalp val #\#)) (error "something at sprinker!"))
	    ((and (= x 500)(= y 0)) (format t "+"))
	    ((equalp val #\#) (format t "#" ))
	    ((equalp val #\~) (format t "~a" #\~ ))
	    (t (format t "." )))))
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
  (let ((min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g)))
    ;; (and (>= x min-x) (<= x min-y)
    ;; 	 (>= y min-y) (<= y max-y))
    (and (>= x min-x) (<= x max-x)
	 (>= y min-y) (<= y max-y))))

(defun at (g x y)
  (cond
    ((on-grid g x y)
     (let ((val (gethash (list x y) (grid-data g))))
       (cond
	 (val val) ;; either #\# clay or #\~ water 
	 (t #\.)))) ;; sand otherwise 
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
;; walls only one item wide , does make a difference ?
;; how determine when end left - right search if no both # each side

(defun overflow (g x y)
  (let ((data (grid-data g)))
    (when (on-grid g x y)
      ;; coming up from #...# clay clay should be water
      ;;(assert (equalp (at g x y) #\~))
      (setf (gethash (list x y) data) #\~)
      (let* ((L (left g x y))
	     (R (right g x y)))
	(cond
	  ((and (equalp L 'clay) (equalp R 'clay)) (overflow g x (- y 1)))
	  (t nil))))))




;; vertically straight down
(defun vertical (g x y)
  (format t "calling vertical at ~a ~a ~%" x y)
  (catch 'kicked-out ;; kicked out water park 
    (let ((data (grid-data g)))
      (catch 'water
	(loop while (on-grid g x y) do
	  (let ((item (at g x y)))
	    (format t "item%% = ~a~%" item)
	    (cond
	      ((equalp item #\#) ; clay
	       (setq y (- y 1))
	       (throw 'water t))
	      ((or (equalp item #\.) (equalp item #\~)) ; sand or water
	       (setf (gethash (list x y) data) #\~)
	       (setq y (+ y 1)))))
	  (when (not (on-grid g x y))
	    (throw 'kicked-out t))))
      (format t "y = ~a~%" y)
      (let* ((L (left g x y))
	     (R (right g x y)))
	(cond
	  ((or (listp L)(listp R))
	   (when (and (not (null L)) (listp L))
	     (destructuring-bind (ign x2 y2) L (vertical g x2 y2)))
	   (when (and (not (null R)) (listp R))
	     (destructuring-bind (ign x2 y2) R (vertical g x2 y2))))
	  (t (overflow g x (- y 1))))))))


(defun left (g x y)
  (catch 'out
    (let ((data (grid-data g)))	  
      (loop while (on-grid g x y) do
	(let ((item (at g x y)))
	  ;; (format t "itemLL = ~a~%" item)
	  (cond
	    ((equalp item #\#) ; hit clay
	     (throw 'out 'clay))
	    ; sand x y - should be water or clay below
	    ((or (equalp item #\.) (equalp item #\~)) 
	     (setf (gethash (list x y) data) #\~)
	     (when (on-grid g x (+ y 1))
	       (let ((item2 (at g x (+ y 1))))
		 (cond
		   ((or (equalp item2 #\~)(equalp item2 #\#))
		    (setq x (- x 1)))
		   (t
		    (setf (gethash (list x y) data) #\~)
		    (vertical g x y)
		    (throw 'out (list 'open x y)))))))))))))





;; what stops us going right ? should always be clay beneath us or water ?
(defun right (g x y)
  (catch 'out
    (let ((data (grid-data g)))	  
      (loop while (on-grid g x y) do
	(let ((item (at g x y)))
	  ;; (format t "itemRR = ~a~%" item)
	  (cond
	    ((equalp item #\#) ; hit clay
	     (throw 'out 'clay))
	    ; sand x y - should be water or clay below
	    ((or (equalp item #\.) (equalp item #\~))
	     (setf (gethash (list x y) data) #\~)
	     (when (on-grid g x (+ y 1))
	       (let ((item2 (at g x (+ y 1))))
		 (cond
		   ((or (equalp item2 #\~)(equalp item2 #\#))
		    (setq x (+ x 1)))
		   (t
		    (setf (gethash (list x y) data) #\~)
		    (vertical g x y)
		    (throw 'out (list 'open x y)))))))))))))


;; (defun find-clay-left (g x y)
;;   (cond
;;     ((equalp (at g x y) #\.) (find-clay-left g (- x 1) y))
;;     ((equalp (at g x y) #\#) t)
;;     (t nil)))

;; (defun find-clay-right (g x y)
;;   (cond
;;     ((equalp (at g x y) #\.) (find-clay-right g (+ x 1) y))
;;     ((equalp (at g x y) #\#) t)
;;     (t nil)))

;; (defun water-left (g x y)  (equalp (at g (- x 1) y) #\~))
;; (defun water-right (g x y)  (equalp (at g (+ x 1) y) #\~))



;; sprinkler located at 500 0
;; but input data when processed says on-grid is at Y=6 onwards ... upto 1631
;; so we need to fake water until enters the on-grid , then we call vertical
(defun shelp (g x y)
  (cond
    ((on-grid g x y)
     (format t "shelp: going vertical at ~a ~a ~%" x y)
     (vertical g x y))
    (t (shelp g x (+ y 1)))))

(defun sprinkler (g)
  (let ((sx 500)(sy 0))
    (shelp g sx sy)))


;; count water tiles
(defun count-tiles (g)
  (let ((min-x (grid-min-x g))
	(max-x (grid-max-x g))
	(min-y (grid-min-y g))
	(max-y (grid-max-y g))
	(data (grid-data g))
	(count 0))
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
	(when (on-grid g x y)
	  (let ((elem (gethash (list x y) data)))
	    (cond
	      ((equalp elem #\~) (incf count)))))))
    count))





(defparameter g (process example))
(defparameter g2 (process input))

(defun run (g)
  (show-grid g)
  (sprinkler g)
  (show-grid g))





;;(trace drip-recur)
;;(untrace)










      



