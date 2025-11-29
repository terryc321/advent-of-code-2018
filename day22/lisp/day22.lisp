;;;; day25.lisp
(ql:quickload :uiop)
(ql:quickload :fiveam)


(uiop:define-package :day22
    (:use :cl :fiveam :uiop))

;;(declaim optimize '(:speed 0 :space 0 :safety 0 :debug 3))
(proclaim '(optimize (speed 0) (space 0) (debug 3)))

(in-package :day22)

;; depth: 11820   :  (0 0) => geo index 0
;; target: 7,782  :  (7 782) => geo index 0

;;current directory
;;(print *default-pathname-defaults*)

(defstruct point
  x
  y
  z
  )

(defparameter *ero* (make-hash-table :test #'equalp))
(defparameter *geo* (make-hash-table :test #'equalp))
(defparameter *reg* (make-hash-table :test #'equalp))
(defparameter *tx* 10)
(defparameter *ty* 10)
(defparameter *depth* 510)
(defparameter *want-cache* nil) 


(defun geo (x y)
  (let ((cache (gethash (list x y) *geo* nil)))
    (cond
      ((and *want-cache* cache)
       ;;(format t "retrieve geo cache value ~a ~%" cache)
       cache)
      (t (let ((result (compute-geo x y)))
	   (setf (gethash (list x y) *geo*) result)
	   result)))))

(defun compute-geo (x y)
  ;; (declare (fixnum x y))
  (cond    
    ((and (= x 0)(= y 0)) 0)
    ((and (= x *tx*) (= y *ty*)) 0)
    ((= y 0) (* 16807 x))
    ((= x 0) (* 48271 y))
    (t (* (ero (- x 1) y)
	  (ero x (- y 1))))))

(defun ero (x y)
  ;;(declare (fixnum x y))
  (let ((cache (gethash (list x y) *ero* nil)))
    (cond
      ((and *want-cache* cache)
       ;;(format t "retrieve ero cache value ~a ~%" cache)
       cache)
      (t (let ((result (compute-ero x y)))
	   (setf (gethash (list x y) *ero*) result)
	   result)))))

(defun compute-ero (x y)  
  ;;(declare (fixnum x y))
  (mod (+ (geo x y) *depth*) 20183))

(defun reg (x y)
  ;;(declare (fixnum x y))
  (let ((e3 (mod (ero x y) 3)))
    (cond
      ((= e3 0) 'rocky)
      ((= e3 1) 'wet)
      (t 'narrow))))

(defun risk (x)
  (cond
    ((eq x 'rocky) 0)
    ((eq x 'wet) 1)
    ((eq x 'narrow) 2)
    (t (error "unknown risk"))))

;;; example depth 510 and target at (10 10)
(defun solve ()
  (let ((tot 0))
    (loop for x from 0 to *tx* do
      (loop for y from 0 to *ty* do
	(setq tot (+ tot (risk (reg x y))))))
    ;; show
    (format t "~%")
    (loop for y from 0 to *ty* do
      (format t "~%")
      (loop for x from 0 to *tx* do
	(let ((r (reg x y)))
	  (cond
	    ((and (= x 0)(= y 0)) (format t "M"))
	    ((and (= x *tx*)(= y *ty*)) (format t "T"))	    
	    ((eq r 'rocky) (format t "."))
	    ((eq r 'wet) (format t "="))
	    ((eq r 'narrow) (format t "|"))
	    (t (error "unknown risk"))))))
    (format t "~%")
    (format t "the total risk is ~a ~%" tot)
    tot))


(defun example ()
  (solve)
  (assert (eq 'rocky (reg 0 0)))
  (assert (eq 'wet (reg 1 0)))
  (assert (= 16807 (geo 1 0)))
  (assert (= 17317 (ero 1 0)))
  
  (assert (= 48271 (geo 0 1)))
  (assert (= 8415 (ero 0 1)))
  (assert (eq 'rocky (reg 0 1)))

  (assert (= 145722555 (geo 1 1)))
  (assert (= 1805 (ero 1 1)))
  (assert (eq 'narrow (reg 1 1)))

  (assert (= 0 (geo 10 10)))
  (assert (= 510 (ero 10 10)))
  (assert (eq 'rocky (reg 10 10)))
  
  )



(defun part1 ()
  (let ((*ero* (make-hash-table :test #'equalp))
	(*geo* (make-hash-table :test #'equalp))
	(*reg* (make-hash-table :test #'equalp))
	(*tx* 7)
	(*ty* 782)
	(*depth* 11820)
	(*want-cache* t))
    (solve)))


#|

DAY22> (time (part1))


M|=.=.|=
.||===|=
=|=.|.||
=.|=.=||
|.|=.=|.
|=.|||||
|=||..||
...||||=
..||.=.=
=.|.==..
==|.=.=|
=.|==|.=
|.=.==||
||..=..|
.|...|.=
..=|.=.|
.|=.|==.
==|.|=.|
==..=|||
||.=.=.|
|..=..|.
|=.|=...
.==|..=|
.=..=.||
.==||=||
=...|=|=
=|..||.|
||.|.=|.
|.|.=.==
|==.=||.
.||..||=
.=|||.|=
==|=...=
==|.|.|.
=|=|=.=.
||=.|=|.
||==|=|.
.=|.====
..|=|..|
.|.==|.|
==|==..=
=|||.|||
|=.|.=|.
|====|==
|=|.|=..
.||=||..
..=||=.=
.|.|||=|
=||==|=|
==.||==.
|=..=...
|=.|..||
|===|=||
.=|=|=.=
.|===|..
==.|=.|=
=....|=.
=|..=|==
|....=||
||.=.|||
..|=.|.=
..|=||||
..=..|.=
=.=....=
=.|.||=.
|=||.=|=
||=...||
|.|==|=.
..|..||.
.=..|==.
=..|.=..
===|==||
==|=|.=.
|.=||===
|=||====
|||=|.==
.=|.||..
...||.||
=||...||
=..=|=|=
=.==|.||
|==||.=.
|||=||.|
.==|====
.===...=
.==.=|.=
=|||||.|
=..|||==
|=.=||||
|=.|.|||
||..=||=
..||...=
.=|=||=|
=|..|.|.
=..||==.
=|=||.==
||..|.|=
|==.|===
||.||..|
.=.=.||=
.|.||..=
==.|=..=
=.=|.||=
=.||=||.
|||||=.=
||.==|..
...=..|=
.=.=|..=
.|.=||=.
=|=.|===
=.|.|==|
|.|=|.|=
||=|.=.|
|.|.|.=.
.==.=|=.
.|=..||=
=|.|===.
=|=||..=
==.||=|=
|.|=....
||=|.=||
||==.|=.
.|=|=...
.=.==||=
=.==.|==
===.=.||
===|=.=.
|.|=.||=
|..|.=||
.=...=..
.====||=
.=|=|==|
=|==||.=
==..|||=
|||||||=
|==|==.=
|=|=..=.
.|=|...=
.=|====.
=|....==
=|.=||||
===.|.|=
|.|.=.=|
|||.==|.
|.|=..=.
.|.|=|||
.==||=..
===..|=|
=||.=|||
=.=|.===
|=|=|..=
|.=...|=
.|.|||.|
....=|.=
..|=.=..
==..=|==
===|==..
|=|=.|=|
|.=..===
||||=|..
.|..=...
..||||..
==.|||||
=..|=|=.
===|||..
|=|==..=
|=|==||.
|.=.=|=|
..===||=
.==||.||
==..==|=
=||||=.|
=||=.==.
|.=.=..=
|===|=|=
.===.=..
..||=..|
.|..|.|.
===.===|
=...|=|=
||||====
||===|||
|=||..|=
.||...||
.==|=.==
=|||||.=
=.|.==..
=||=..||
||.||=|=
|||.||=|
.==|==||
..=.|=.=
..||||..
=||.=|==
=======.
==...|==
||||==|=
|.=.|...
.|==.==.
.|=|.|.=
...|||=|
=|...|..
=.|||==.
|.|==.|.
|.|..=|.
|===|.==
..===||=
.==|.|.=
==.|=.|=
=.|.....
===..=|=
|.|=|===
|.=|.=.|
..|==..|
.==|....
....==..
==..=|=|
=.=..|=|
==|=..||
||==..=.
|||==.=|
.==..|||
.|..|..|
.==|=.|.
==.|=.|=
==..|.|.
|.==..||
|.|=..||
||..=||.
.=...=|=
.....=|.
=.=.||==
==.==.=.
=....=..
|.=.|=|=
||=.|=..
.=|.|.|=
..=.==||
.|..|.|=
=.|==|||
==..====
=.===|=.
||==.=|=
|..|.=.=
..|=|===
.==|...|
.|=.=...
=|==||.|
=.==|=||
||..||..
|=.==|..
|||=...|
....|==.
..=..==.
=|.|=|=|
=||==..=
=.=|..==
|.||.|==
|.|=====
.=|=.||.
.|.|=.==
.||==.==
=|=|=..|
==|..==.
=|==|.==
||=.==.=
||=....=
.||=.=.=
.==..=.=
.==|.||.
===.|==|
===|..||
|||=..=|
|..=|=.=
|.|||||=
..|.=.|.
..|==|..
=.=|.=..
==|.==.=
==||=|.|
||.|===.
|==|.|.|
..|||.||
.|=|.|=.
.=..=.=|
=.===.|=
=|==..=|
==..|||.
||=|.=..
|=.=|=|.
.|..|=.|
..||.=|.
.|=.|===
====..==
==|||||.
|=|..=|.
|.||==|.
|==.=|.|
.|=.====
.|=|=|.=
==.=|.=|
=|||==|=
==..|.|.
|====.|=
|..====|
...====|
..||=..=
.=|=|.=|
=.||=.=|
==.==..|
|..|||..
|.=|||.=
|...|..=
.===|.|.
.=.||=.|
..|=||||
=|=.==|.
=.=|||==
|.||||..
|=.|=.==
|.||==.=
.||.=..=
...===..
=.||=|..
=..=||.=
=|=||=.|
||.|||=|
|.||=...
.=|.|.=.
.=|=||=.
..||=.|.
=||=|...
=|.=.==|
|=..==.|
|==.=.==
||.|=.==
...=...=
..=.|=.=
.|||==|.
=.=.=.|=
=|=|.||=
|||=.==.
||.==||.
|||.||.=
.==.=|.|
.=|.||.=
=...||=|
=|.|..=.
=.=..=.|
|||..=||
|.|..|.|
.|=..=..
.|=...|.
.||.|.|=
==.=.=|=
=.||=..=
||=|||=|
|.|=.=|=
|==|.=|.
.=|.=..|
..=.=.==
.=.=|==.
==.|.|.|
==......
|.|==|.=
||=|||.|
||=.||..
..|...|.
..|==|||
=.==|...
=..|.=||
=.=.=|=|
|||||||=
|..=|.|.
.|.==...
..=|==|.
.==.||=.
==|===.|
==|=.|..
|=...==|
|=|..=||
|.|=.|==
..=.===|
...|...=
.=|==..=
===|===.
=.==|.|.
|.=||||=
||=.||.=
||.||..=
.=.|.==.
.=.|=||=
===.=.=.
=|=|===.
=.||====
|.==||.=
|==.|.=|
.||=|.|=
..=|....
....=.|.
=..|.|==
==.=..||
|..=.===
|.|.|.=.
|||||=..
..==....
.|=|=.==
.||=||=|
=|||.=||
=.=====|
|=||.==|
|==|.==.
||||.=..
.||..|=.
.==.....
==.=||=.
===|=|||
==|...|=
|.=..||.
|==|==.=
..=|==|=
...|||=.
.|||.=||
=====.==
=..|====
|..||.=|
|..|.|||
||||=...
.|.|.===
.==|...=
==.=|.|.
===||.=.
=====..=
||..||..
|=||=...
|.==|.=|
..|=.|.|
.|||=.|.
==|=|=.|
====.=.|
==.=.===
|=|=|=.|
|=|||==.
..|===.=
.||=.|.|
.|||=|.|
=..=..=|
=.|=.=.|
|=|=.=|=
|||=..|=
|.=.=||.
.=.|||.|
.==||..=
=.|=..|.
=====||=
=|.|=..=
|||||.=|
||.||=|.
|.|=||=|
.===..||
..|.=|=|
====.==|
=|||=.==
==|.=.|=
|..==..|
|...=|==
...|=||.
.=|...==
...==|.=
==|.=.=|
=|.|=|.=
||=.=|=|
|==||=|=
|====.|.
.|=.==..
.||===.|
=..=||=|
=|=..=||
===|====
|.=.|||.
|||=|=||
|.==|.=|
.||=|===
.=|.==.=
==.|=.||
===.|.=|
=..||.|.
||..=|||
|.=|=|.|
.=||===.
.|.|..||
.=|==..=
=|==||=.
=.===|==
|===.|.|
|===|=.|
||=|.|=|
.==.=.=|
...==..|
===|.===
===...|.
=.|.|..|
|.|.|=|=
|..|.|.=
|=.=|=.=
.|.==.||
.|..=|=|
=||||=||
==||=.=|
=.|.=|||
|=.|.=.|
||.|.=|=
.=|.=.=|
.|=|=.||
..|..=..
=.=.=.==
=.==.===
|=.|.|||
||==....
||.==...
....|=..
..=||..=
==||..||
==||.===
===|||==
|.=||=.|
||.|..==
||...|..
.==..||=
.=||==.|
=.=|==||
==..=|..
=.|=|=.|
|..==..=
||||.|==
...|.|=|
.=.|.=|=
.=.|=|||
=||||.|=
==.|=.=|
|.|||.|=
|=.=.=|=
|||.=|||
.=.|=|=.
...|.|||
=||=|=..
=||.=.==
=|.|=.||
|===||.=
|=|=...|
.||||=||
.=||=.|.
.==|.=..
===|.||=
===|=.|.
=|.||.||
|==...=|
|=|=.==|
.===.|.|
.|||.=|=
.|=|.==|
=|==||=|
=|=.=.=.
|==.=|.=
|.=....=
|.||=.|=
..||=.|=
.===.==.
===|=.=|
==|=|=.=
=.=.=|=|
|||||=..
|=.=|=.|
...|=|||
.||==|||
.==|===.
=||=.==.
=.|.=.=.
==.|..||
|.||=|.=
|....||.
.|..||=|
.=.===..
..==|=..
==||=.=.
==.|.|=|
|.|=.=|=
||.=.||.
||===.|=
.=||=.||
.|=|.=..
=.|=.=.|
=|==.==|
=.==.=|.
||====.|
|...=.|=
.|||.|||
.|==.=|=
..||.=||
==|===|=
=||.||..
=..||..|
|=.==|=.
|==|....
..|..|.=
.|=====.
..|.||=.
==|||..=
=.|=|..=
||.==..|
|||=.|..
|===.||=
.=..==|.
.=.==||.
=||.==.|
===|.|=.
====|||.
|||==.|.
||=.|=||
.=....|=
.==|.==.
.=|.===|
=||.|.=.
==|.=.==
=.||.|=.
||.==||.
||=..==|
..|==|.=
..=.=.|=
.||=|||.
=|.===||
=||=.|..
|||=||=|
|.==|...
|=.||=..
.||==||.
.=||..=|
==|=.=.=
==..|=|.
=..==|||
||==...|
|===|.|.
....=|.|
.==.=..|
..|.=.||
=|=||.=|
=|..|||.
=.|=||.=
|||.=||=
||.=.==|
.||.|=.|
.|||=|=.
..=..|=|
=.=.==.=
=.=.=.=.
|...=|||
|===|.=.
|||..==.
.......=
.|..==|=
=..|.===
==..|..=
=.|.|.|.
|.||||||
||||||=|
.===||==
.=|=|.||
..=|.|..
=|=||.|=
=.=.==.=
|=..||=|
|.||.||.
|=.=|||=
.===|||.
...|.||.
.=..=..|
===..|.|
==..=..|
||=|.|..
|=||...|
||.||=.=
..||.=|.
..|||..|
=.|.||==
=...||=.
====.|.|
|=.=.=||
|=|.==.|
....||..
..|||..=
..=|.=.=
=|||=|==
=.||||=|
|=|===..
|...||||
|.=||==.
..|=|==.
..=..=..
.|==|...
=|==|||.
=.=..==|
|.||=...
|.|.=..|
|.||.===
.=.=|.|.
.=|.|==.
=..=|=||
=.||||.=
==.|=|=.
||=.==|.
|.|=|.=|
.=|.==..
...===||
.||=.|..
=..==|||
==..=.||
|=|..||=
|..=|..=
|.===...
..|.=..|
.=|=||.|
....|=|.
=|||==.|
=||.|.|.
|.|==||=
|.|||||.
|.=|..|=
.|||=.|.
.||.....
===....|
====|..|
=|.=|=..
|==||.=|
||||.||=
.|.|.=||
.===|..=
.=.=|||=
=|.|....
==|..=|.
|.|=.===
|.|=.=|.
||..=...
..|.=.=.
.=||=|||
.=|.=.|=
=.==|...
=|..=|||
|.|..==|
|=..|=||
|=.|.|..
.||.||.|
.=|=|===
=|=.=..=
=..|.|||
=|||=|.|
|=.|==|.
|.=|=...
.|.|==|.
.===..=.
..||==..
=|.||.==
=|=||..|
||=|====
||.===.=
|.||===|
.=.==|||
..|||...
.|=.=|=.
==|=.=.=
==..===|
|.|=||=.
|=...===
|=..=.=.
.||=|.||
..|.=|||
===.=||.
=||=|==|
====||.|
|=||==.=
|=.=....
..|=||=|
..|..|..
.=|.===T
the total risk is 6318 
Evaluation took:
  0.005 seconds of real time
  0.005454 seconds of total run time (0.005440 user, 0.000014 system)
  100.00% CPU
  19,952,894 processor cycles
  3,014,144 bytes consed
  
6318
answer accepted 

|#

#+nil
dummy-expr1

#+nil
dummy-expr2


#+nil
 (error "foo")
  
;; ============================= Test Suite =================================


;; Define a suite and set it as the default for the following tests.
(fiveam:def-suite test  :description "Test the star functions")
(fiveam:in-suite test)

(fiveam:test bots-0
  (fiveam:is (equalp (make-nano :strength 4
				:point #(1 2 3))
		     (nano-make '(1 2 3 4)))))


(defun run-tests ()
  (fiveam:run! 'test))


#|
The scan (your puzzle input) is not very detailed: it only reveals the depth of the cave system and the coordinates of the target. However, it does not reveal the type of each region. The mouth of the cave is at 0,0.

The man explains that due to the unusual geology in the area, there is a method to determine any region's type based on its erosion level. The erosion level of a region can be determined from its geologic index. The geologic index can be determined using the first rule that applies from the list below:

    The region at 0,0 (the mouth of the cave) has a geologic index of 0.
    The region at the coordinates of the target has a geologic index of 0.
    If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.
    If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.
    Otherwise, the region's geologic index is the result of multiplying the erosion levels of the regions at X-1,Y and X,Y-1.

A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183. Then:

    If the erosion level modulo 3 is 0, the region's type is rocky.
    If the erosion level modulo 3 is 1, the region's type is wet.
    If the erosion level modulo 3 is 2, the region's type is narrow.

For example, suppose the cave system's depth is 510 and the target's coordinates are 10,10. Using % to represent the modulo operator, the cavern would look as follows:

    At 0,0, the geologic index is 0. The erosion level is (0 + 510) % 20183 = 510. The type is 510 % 3 = 0, rocky.
    At 1,0, because the Y coordinate is 0, the geologic index is 1 * 16807 = 16807. The erosion level is (16807 + 510) % 20183 = 17317. The type is 17317 % 3 = 1, wet.
    At 0,1, because the X coordinate is 0, the geologic index is 1 * 48271 = 48271. The erosion level is (48271 + 510) % 20183 = 8415. The type is 8415 % 3 = 0, rocky.
    At 1,1, neither coordinate is 0 and it is not the coordinate of the target, so the geologic index is the erosion level of 0,1 (8415) times the erosion level of 1,0 (17317), 8415 * 17317 = 145722555. The erosion level is (145722555 + 510) % 20183 = 1805. The type is 1805 % 3 = 2, narrow.
    At 10,10, because they are the target's coordinates, the geologic index is 0. The erosion level is (0 + 510) % 20183 = 510. The type is 510 % 3 = 0, rocky.

|#
