;; 
;; load the cairo-sdl ffi interface 
;; (load "/home/terry/code/c2ffi/cairo-sdl/register-asd.lisp")
;;


#|

a way to visualise the 2 d grid the trains are running on
my input text is 150 x 150
want to be able to zoom in , move around the grid ,
advance the grid - time step forward - ie move every single train one step ,
inform us if there is a train collision or otherwise
back the grid - time step backward - ie move every single train back one step ,

issues

- text is reprinted to screen becomes blurred because background is not properly cleared by a
rectangle draw


how define screen zoom ?

talking about size of each grid ?

do we have a definite border on each grid ?

do we render whole grid if only looking at small portion of grid ?


- no clipping , we ask cairo to draw at negative x , y and fingers cross it does not break cairo

- hash table that records changes ?
need a time step with hash record ?
how will it know to undo ?



|#



;; limit to number of squares that can be accomodated


;; do not know how this packaging stuff works really , cannot just load a file
;; surely be eaiser to wrap whole file in a big letrec

;; (load "register-asd.lisp")

;; (defpackage #:cairo-balls
;;   (:use #:cl #:cairo-sdl))

(in-package #:cairo-sdl)


(declaim (optimize (speed 0)(debug 3)(safety 3)))


;; ------------ various screen sizes --------------------------

;; (defparameter *screen-width* 1024)
;; (defparameter *screen-height* 960)

;; (defparameter *screen-width* 1920)
;; (defparameter *screen-height* 1080)

(defparameter *screen-width* 1900)
(defparameter *screen-height* 1000)
(defparameter *window-title* "train visualiser")
(defparameter *font-size* 12d0)

;; ----------- mouse x y defined in c2ffi/cairo-sdl.lisp ----------
;; (defparameter *mouse-x* 0)
;; (defparameter *mouse-y* 0)
(defparameter *last-key-event-string* "none")
;; preserve square appearance of grid on screen

(defparameter *screen-grid-box-width* 60d0)
(defparameter *screen-grid-box-height* 60d0)

(defparameter *min-screen-grid-box-width*  1d0)
(defparameter *min-screen-grid-box-height* 1d0)


;; 0d0 no border
(defparameter *screen-grid-box-border-width* 1d0)
(defparameter *screen-grid-box-border-height* 1d0)

(defparameter *screen-grid-offset-x* 50d0)
(defparameter *screen-grid-offset-y* 60d0)

(defparameter *screen-grid-zoom* 1.0d0)

;; -------------problem specific data structures ------------------------
(defparameter *train-track* nil)
(defparameter *train-history* nil)
(defparameter *global-train-tick* 0)
(defparameter *global-last-train-tick* 0)


;; --------------------------------------------------------

;; -------- x increases to right
;; -------- y increases down ... inversion of math principles ...? 
(defparameter *grid-top-x* 0)
(defparameter *grid-top-y* 0)








;; ---------------------------------------------------------

;; some weird representation of keyboard as an array
(defparameter *keys* (make-array 512 :initial-element nil))

;; ---------old stuff balls , animation ? ------------------

;; (defparameter ball-1 (make-ball))
;; (defparameter ball-2 (make-ball))
;; (defparameter ball-3 (make-ball))

(defparameter n-balls 1)
(defparameter balls (make-array (+ 1 n-balls)))

;;(macro-expand-1 '(draw-ball ball-1))

(defparameter time-then (get-internal-real-time))
(defparameter time-now 0)
(defparameter time-difference 0)
(defparameter time-step 0.01)

;; --------------------------------------------------------


;;(screen-format x y msg args )

(defmacro screen-format (x y msg . args)
  `(progn
     (cairo-set-font-size cr (+ 0.0d0 *font-size*))
     (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
     (cairo-move-to cr (+ 0d0 ,x) (+ 0d0 ,y))
     ;; no need for a newline at end of string as it is not understood in this context
     (cairo-show-text cr (format nil ,msg ;;"ball details : ~a ~a : vel ~a ~a "
				 ,@args
				 ;; (second (assoc 'x ball-1))
				 ;; (second (assoc 'y ball-1))
				 ;; (second (assoc 'vx ball-1))
				 ;; (second (assoc 'vy ball-1))
				 ))))



;; from old days drawing keyboard key on screen 
(defmacro draw-key (index x y label)
  `(progn
     (if (aref *keys* ,index)
	 (cairo-set-source-rgb cr 1.0d0 0.0d0 0.0d0)
	 (cairo-set-source-rgb cr 0.0d0 0.0d0 1.0d0)
	 )
     (cairo-rectangle cr (+ 0.0d0 ,x) (+ 0.0d0 ,y) 25d0 25d0)
     (cairo-fill cr)
     (cairo-select-font-face cr "serif" +cairo-font-slant-normal+ +cairo-font-weight-bold+)
     (cairo-set-font-size cr 12.0d0)
     (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
     (cairo-move-to cr (+ 2.0d0 ,x) (+ 16.0d0 ,y))
     (cairo-show-text cr ,label)
     ))

;; conversion to double floating point for FFI or else all hell breaks loose
;; not a ball as such , just a rectangle
(defmacro draw-ball (ball)
  `(progn
     (let ((x (second (assoc 'x ,ball)))
	   (y (second (assoc 'y ,ball)))
	   (r (second (assoc 'red ,ball)))
	   (g (second (assoc 'green ,ball)))
	   (b (second (assoc 'blue ,ball)))
	   (size (second (assoc 'size ,ball))))	   
     (cairo-set-source-rgb cr (+ 0.0d0 r) (+ 0.0d0 g) (+ 0.0d0 b))
     (cairo-rectangle cr (+ 0.0d0 x) (+ 0.0d0 y) (+ 0.0d0 size) (+ 0.0d0 size))
     ;;(cairo-circle cr (+ 0.0d0 ,x) (+ 0.0d0 ,y) 25d0 25d0)
     (cairo-fill cr)
       )))


(defun non-zero-random-number (n)
  (let ((rnd 0))
    (catch 'done
      (loop while t do
	(setq rnd (- (random (+ 1 (* 2 n))) n))
	(when (not (zerop rnd))
	  (throw 'done rnd))))))



(defun make-ball ()
  (let ((w *screen-width*)
	(h *screen-height*))
    `((x ,(random w))
      (y ,(random h))
      (vx ,(non-zero-random-number 20))
      (vy ,(non-zero-random-number 20))
      (size ,(+ 25 (random 5)))
      (red ,(random 1.0))
      (green ,(random 1.0))
      (blue ,(random 1.0)))))


;;
(defun tick-ball (ball time-step)
  (let ((w *screen-width*)
	(h *screen-height*)
	(x (second (assoc 'x ball)))
	(y (second (assoc 'y ball)))
	(vx (second (assoc 'vx ball)))
	(vy (second (assoc 'vy ball)))	   
	(r (second (assoc 'red ball)))
	(g (second (assoc 'green ball)))
	(b (second (assoc 'blue ball)))
	(size (second (assoc 'size ball))))
    
    (setq x (+ x (* time-step vx)))
    (setq y (+ y (* time-step vy)))
    
    (cond
      ((< x size) (setq vx (- vx)))
      ((> x (- w size)) (setq vx (- vx))))
    
    (cond
      ((< y size) (setq vy (- vy)))
      ((> y (- h size)) (setq vy (- vy))))
    
    `((x ,x)
      (y ,y)
      (vx ,vx)
      (vy ,vy)
      (size ,size)
      (red ,r)
      (green ,g)
      (blue ,b))))






(defun reboot-balls ()
  (when (< n-balls 1)
    (setq n-balls 1))    
  (setq balls (make-array n-balls))
  ;; make 10,000 balls
  (loop for i from 0 to (- n-balls 1) do
    (setf (aref balls i) (make-ball))))


(defun clear-screen (cr)
  (cairo-set-source-rgb cr 0.0d0 0.0d0 0.0d0)      
  ;;(cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)      
  (cairo-rectangle cr  0d0  0d0 (+ 0d0 *screen-width*)
		   (+ 0d0 *screen-height*))
  (cairo-fill cr))




(defun draw-blank-square (dx dy cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    ;;(cairo-set-source-rgb cr (+ 0.0d0 red 1.0d0) (+ 0.0d0 green 1.0d0) (+ 0.0d0 blue 1.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red 0.0d0) (+ 0.0d0 green 0.0d0) (+ 0.0d0 blue 0.0d0))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    ;;(cairo-circle cr (+ 0.0d0 ,x) (+ 0.0d0 ,y) 25d0 25d0)
    (cairo-fill cr)
    ))


(defun draw-cross-square (dx dy cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))

    ;; black background ?
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)

    ;; horz
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy (/ *screen-grid-box-height* 3.0d0))
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 (/ *screen-grid-box-height* 3.0d0)))

    ;; vert
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 dy)
		     (+ 0.0d0 (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 *screen-grid-box-height*))
    
    ;;(cairo-circle cr (+ 0.0d0 ,x) (+ 0.0d0 ,y) 25d0 25d0)
    (cairo-fill cr)
    ))


;;   C 
;;  B
;; A 
(defun draw-slash-square (dx dy cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))

    ;; black background ?
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)

    ;; A
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx )
		     (+ 0.0d0 dy (* 2.0d0 (/ *screen-grid-box-height* 3.0d0)))
		     (+ 0.0d0  (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0  (/ *screen-grid-box-height* 3.0d0)))
    (cairo-fill cr)
    
    ;; B
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 dy (/ *screen-grid-box-height* 3.0d0))
		     (+ 0.0d0 (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 (/ *screen-grid-box-height* 3.0d0)))
    (cairo-fill cr)

    
    ;; C
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx (* 2.0d0 (/ *screen-grid-box-width* 3.0d0)))
		     (+ 0.0d0 dy )
		     (+ 0.0d0 (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 (/ *screen-grid-box-height* 3.0d0)))
    (cairo-fill cr)
    ))






;; A
;;  B 
;;    C
(defun draw-backslash-square (dx dy cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))

    ;; black background ?
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)

    ;; A
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx )
		     (+ 0.0d0 dy )
		     (+ 0.0d0  (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0  (/ *screen-grid-box-height* 3.0d0)))
    (cairo-fill cr)
    
    ;; B
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 dy (/ *screen-grid-box-height* 3.0d0))
		     (+ 0.0d0 (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 (/ *screen-grid-box-height* 3.0d0)))
    (cairo-fill cr)

    
    ;; C
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx (* 2.0d0 (/ *screen-grid-box-width* 3.0d0)))
		     (+ 0.0d0 dy (* 2.0d0 (/ *screen-grid-box-height* 3.0d0)))
		     (+ 0.0d0 (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0 (/ *screen-grid-box-height* 3.0d0)))
    (cairo-fill cr)

    
    ))


(defun draw-vert-square (dx dy cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))

    ;; black background ?
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)

    ;; A
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx (/ *screen-grid-box-width* 3.0d0) )
		     (+ 0.0d0 dy )
		     (+ 0.0d0  (/ *screen-grid-box-width* 3.0d0))
		     (+ 0.0d0  *screen-grid-box-height*))
    (cairo-fill cr)

    ))



(defun draw-horz-square (dx dy cr)
    (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))

    ;; black background ?
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)

    ;; A
    (cairo-set-source-rgb cr (+ 0.0d0 red 1.0) (+ 0.0d0 green) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx )
		     (+ 0.0d0 dy (/ *screen-grid-box-height* 3.0d0))
		     (+ 0.0d0  *screen-grid-box-width*)
		     (+ 0.0d0 (/ *screen-grid-box-height* 3.0d0 )))
    (cairo-fill cr)
      ))






(defun draw-train-square-left (dx dy tint cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))
    
    ;; green background 
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green 1.0d0) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)
    ))

  


(defun draw-train-square (dx dy tno tdir tint cr)
  (let ((red 0.0d0)
	(green 0.0d0)
	(blue 0.0d0))
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green) (+ 0.0d0 blue))

    ;; green background 
    (cairo-set-source-rgb cr (+ 0.0d0 red) (+ 0.0d0 green 1.0d0) (+ 0.0d0 blue))
    (cairo-rectangle cr
		     (+ 0.0d0 dx)
		     (+ 0.0d0 dy)
		     (+ 0.0d0 *screen-grid-box-width*)
		     (+ 0.0d0 *screen-grid-box-height*))
    (cairo-fill cr)

    (cond
      ((eq tdir 'left) (draw-train-square-left dx dy tint cr))
      ;; ((eq? tdir 'right) (draw-train-square-right dx dy tint cr))
      ;; ((eq? tdir 'up) (draw-train-square-up dx dy tint cr))
      ;; ((eq? tdir 'down) (draw-train-square-down dx dy tint cr))
      )))



;; keep train tick within known good values - ie where always a train on the track
(defun next-train-tick ()
  (incf *global-train-tick*)
  (when (> *global-train-tick* *global-last-train-tick*)
    (setq *global-train-tick* *global-last-train-tick*)))
  

(defun prev-train-tick ()
  (decf *global-train-tick*)
  (when (< *global-train-tick* 0)
    (setq *global-train-tick* 0)))



;;
;;
;; infinite drawing area 
;;


(defun draw-screen-grid (cr)
  ;; lookup trains outside the loop , then just run over 17 trains each square we draw
  (let* ((dx 0)
	 (dy 0)
	 (red 0.0d0)
	 (green 0.0d0)
	 (blue 0.0d0)
	 (trains (gethash *global-train-tick* *train-history*))
	 )
    (setq dy *screen-grid-offset-y*)
    (loop for y from 0 to 150 do
      (setq dx *screen-grid-offset-x*)
      
      (loop for x from 0 to 150 do


	;; black background
	(cairo-set-source-rgb cr (+ 0.0d0 red 0.0d0) (+ 0.0d0 green 0.0d0) (+ 0.0d0 blue 0.0d0))
	
	;; white background
	;;(cairo-set-source-rgb cr (+ 0.0d0 red 1.0d0) (+ 0.0d0 green 1.0d0) (+ 0.0d0 blue 1.0d0))
	
	;; dispatch on type of train track square we need to draw 
	(let* ((sq (list (- x *grid-top-x*)  (- y *grid-top-y*)))
	       (at (gethash sq *train-track*)))
	  ;;(format t "draw-screen-grid [~a ~a : ~a]~%" x y at)
	  (cond
	    ((eq at 'cross) (draw-cross-square dx dy cr))
	    ((eq at 'slash) (draw-slash-square dx dy cr)) 
	    ((eq at 'backslash) (draw-backslash-square dx dy cr)) ;; todo 
	    ((eq at 'vert) (draw-vert-square dx dy cr));; todo
	    ((eq at 'horz) (draw-horz-square dx dy cr));; todo
	    (t (draw-blank-square dx dy cr))
	    ))

	  (dolist (train trains)
	    (destructuring-bind ((train-no tno)(xx tx)(yy ty)(direction tdir)(internal tint)) train
	      (when
		  (and (= (- x *grid-top-x*) tx) (= (- y *grid-top-y*) ty))
		(draw-train-square dx dy tno tdir tint cr))))
	
	;; 
	(incf dx *screen-grid-box-border-width*)
	(incf dx *screen-grid-box-width*)
	    ) ;; for x
      
      (incf dy *screen-grid-box-border-height*)
      (incf dy *screen-grid-box-height*)
      ) ;; for y 
      
    ))






(defun zoom-enlarge ()
  (setq *screen-grid-box-width* (+ 1 *screen-grid-box-width*))
  (setq *screen-grid-box-height* (+ 1 *screen-grid-box-height*))
  )


(defun zoom-shrink ()
  (setq *screen-grid-box-width* (+ -1 *screen-grid-box-width*))
  (setq *screen-grid-box-height* (+ -1 *screen-grid-box-height*))

  (when (< *screen-grid-box-width* *min-screen-grid-box-width*)
    (setq *screen-grid-box-width* *min-screen-grid-box-width*))

  (when (< *screen-grid-box-height* *min-screen-grid-box-height*)
    (setq *screen-grid-box-height* *min-screen-grid-box-height*))

  
  )



;; remember to be in ;; (in-package :cairo-sdl)
(defun load-output-file ()
  (let ((tick nil)
	(known nil))
    (setq *train-track* (make-hash-table :test #'equalp))
    (setq *train-history* (make-hash-table :test #'eql))
    (cl:with-open-file (stream "output" :direction :input)
      (catch 'done
	(loop while t do
	  (let ((in (read stream nil)))
	    ;; (format t "in = [~a]~%" in)
	    ;; track ...
	    ;; (when (and (consp in) (eq (first in) 'track))
	    ;;   (format t " ... TRACK piece found ~%"))
	    (cond
	      ((null in) (throw 'done t))
	      ;; train track 
	      ((and (consp in) (eq (car in) 'track) (fourth in)) ;; exclude empty track pieces
	       ;; (format t "track ~A ..." in)
	       ;;(format t "(4th=> ~A) ~%" (fourth in))
	       (let ((x (second in))
		     (y (third in))
		     (at (fourth in)))
		 (setf (gethash (list x y) *train-track*) at)
		 ))
	      ((and (consp in) (eq (car in) 'tick))
	       ;; record trains in previous tick , locations etc..
	       (when tick
		 (setf (gethash tick *train-history*) known))
	       (setq tick (second in))
	       (setq *global-last-train-tick* tick)
	       (setq known '())
	       ;;(format t "tick ~a ~%" tick)
	       )
	      ((and (consp in) (consp (car in)) (assoc 'train-no in)) ;; train data
	       (setq known (cons in known))
	       ;;(format t "~a => ~a~%" in tick)
	       )
	      (t
	       ;;(format t ";;?? IGNORED ~a~%" in)
	       t
	       ))))))))






(defun trains-demo ()

  (load-output-file)
  
  ;; (reboot-balls)
  
  (sdl-init (logior +sdl-init-video+ +sdl-init-events+))
  (img-init (logior +img-init-png+))
  (setq window (sdl-createwindow *window-title*
				 0
				 0				   
				 *screen-width*
				 *screen-height*
				 (logior
				      ;;+sdl-window-fullscreen-desktop+
				      +sdl-window-shown+
				      ;;+sdl-window-borderless+
				      +sdl-window-allow-highdpi+
				      ;;+sdl-window-fullscreen+
				      +sdl-window-resizable+
				      )))

  (setq render (let ((render-flags (logior
				    ;;+sdl-renderer-software+
				    ;;+sdl-renderer-presentvsync+
				    +sdl-renderer-accelerated+
				    +sdl-renderer-targettexture+
				    ))
		     (index -1))
		 (sdl-createrenderer window index render-flags)))
  (let ((width *screen-width*)
	(height *screen-height*)
	(flags 0) ;; flags always 0 unused
	(depth 32) 
	(red-mask #x00FF0000)
	(green-mask #x0000FF00)
	(blue-mask #x00000FF)
	(alpha-mask 0))
    (setq sdl-surface (sdl-create-rgb-surface flags
					      width
					      height
					      depth
					      red-mask
					      green-mask
					      blue-mask
					      alpha-mask)))
  
  (let* ((pitch (cffi:foreign-slot-value sdl-surface '(:struct sdl-surface-struct) 'pitch))
	 (width (cffi:foreign-slot-value sdl-surface '(:struct sdl-surface-struct) 'width))
	 (height (cffi:foreign-slot-value sdl-surface '(:struct sdl-surface-struct) 'height))
	 (pixels (cffi:foreign-slot-value sdl-surface '(:struct sdl-surface-struct) 'pixels))
	 ;;(stride (cairo-format-stride-for-width +cairo-format-rgb24+ width))
	 )

    ;; create cairo surface 
    (setq cairo-surface (cairo-image-surface-create-for-data			   
			 pixels
			 +cairo-format-rgb24+
			 width
			 height
			 pitch
			 )))
  
  (setq cr (cairo-create cairo-surface))

  (setq *close* nil)


  (cairo-select-font-face cr "DejaVuSans" +cairo-font-slant-normal+ +cairo-font-weight-normal+)     
  ;;(cairo-select-font-face cr "bullshido" +cairo-font-slant-normal+ +cairo-font-weight-normal+)
      

  
  ;; int4 * 64 => 256 bytes , only 56 bytes needed for event
  (cffi:with-foreign-object (ev-ptr :int 64) 
    (loop while (not *close*) do


      ;; clears the screen in jet black 
      (clear-screen cr)
      
      (setq time-now (get-internal-real-time))
      (setq time-difference (- time-now time-then))

      ;;(format t "time difference ~a ~%" time-difference)
      
      ;; (when (> time-difference 1000000) ;; ------------------------- renderer held hostage 

      ;; 	;; update clock 
      ;; 	(setq time-then (get-internal-real-time))

	
      ;; -------- clear screen - draw a black background  ---------
      ;;(clear-screen cr)
      
      ;; -------- move the balls - bounce off walls - bounce off each other ------

      ;; -------- tick ? --------
      ;; (setq ball-1 (tick-ball ball-1 time-step))
      ;; (setq ball-2 (tick-ball ball-2 time-step))
      ;; (setq ball-3 (tick-ball ball-3 time-step))


      ;; -------------------- 
      ;; ;; tick each ball
      ;; (loop for i from 0 to (- n-balls 1) do 
      ;; 	(setf (aref balls i) (tick-ball (aref balls i) time-step))
      ;; 	(draw-ball (aref balls i)))
      
      ;; --------------- drawing code goes here ----------------------
      ;;(draw-ball ball-1)
      ;;(draw-ball ball-2)      
      ;;(draw-ball ball-3)
      

      
      ;; some text information on --------------
      ;;(cairo-select-font-face cr "helvetica" +cairo-font-slant-normal+ +cairo-font-weight-bold+)
      ;;(cairo-select-font-face cr "helvetica" +cairo-font-slant-normal+ +cairo-font-weight-normal+)
      ;;(cairo-select-font-face cr "comic sans" +cairo-font-slant-normal+ +cairo-font-weight-normal+)
      ;;(cairo-select-font-face cr "dejavu" +cairo-font-slant-normal+ +cairo-font-weight-normal+)
      ;;(cairo-select-font-face cr "DejaVuSansMono" +cairo-font-slant-normal+ +cairo-font-weight-normal+)

      
      
      ;;(screen-format x y msg args )

      (cairo-set-source-rgb cr (+ 0.0d0) (+ 0.2d0 ) (+ 0.0d0 ))
      (cairo-rectangle cr (+ 0.0d0 20.0) (+ 0.0d0 0.0) (+ 0.0d0 250.0) (+ 0.0d0 *font-size*))
      (cairo-fill cr)
      (screen-format 20 10 "window size ~a ~a : "    *screen-width* *screen-height*)

      (cairo-set-source-rgb cr (+ 1.0d0) (+ 0.0d0 ) (+ 0.0d0 ))
      (cairo-rectangle cr (+ 0.0d0 10.0) (+ 0.0d0 10.0) (+ 0.0d0 200.0) (+ 0.0d0 *font-size*))
      (cairo-fill cr)
      (screen-format 20 20 "mouse position ~a ~a : "    *mouse-x* *mouse-y*)
      
      (cairo-set-source-rgb cr (+ 0.0d0) (+ 0.0d0 ) (+ 1.0d0 ))
      (cairo-rectangle cr (+ 0.0d0 20.0) (+ 0.0d0 20.0) (+ 0.0d0 200.0) (+ 0.0d0 *font-size*))
      (cairo-fill cr)
      (screen-format 20 30 "last key event ~a : "    *last-key-event-string*)

      (cairo-set-source-rgb cr (+ 0.0d0) (+ 0.0d0 ) (+ 1.0d0 ))
      (cairo-rectangle cr (+ 250.0d0 20.0) (+ 0.0d0 20.0) (+ 0.0d0 200.0) (+ 0.0d0 *font-size*))
      (cairo-fill cr)
      (screen-format 250 30 "current tick [ ~a of ~a ]"    *global-train-tick* *global-last-train-tick*)



      
      
      ;; ;; (cairo-set-source-rgb cr (+ 0.0d0) (+ 0.2d0 ) (+ 0.0d0 ))
      ;; ;; (cairo-rectangle cr (+ 0.0d0 40.0) (+ 0.0d0 40.0) (+ 0.0d0 250.0) (+ 0.0d0 *font-size*))
      ;; ;; (cairo-fill cr)
      ;; (screen-format 50 50 "ball details : ~a ~a : vel ~a ~a "
      ;; 		     (second (assoc 'x ball-1))
      ;; 		     (second (assoc 'y ball-1))
      ;; 		     (second (assoc 'vx ball-1))
      ;; 		     (second (assoc 'vy ball-1)))
      
     ;; (cairo-set-font-size cr (+ 0.0d0 *font-size*))
     ;; (cairo-set-source-rgb cr 1.0d0 1.0d0 1.0d0)
     ;; (cairo-move-to cr (+ 0d0 20) (+ 0d0 20))
     ;; ;; no need for a newline at end of string as it is not understood in this context
     ;; (cairo-show-text cr (format nil "ball details : ~a ~a : vel ~a ~a "
     ;; 				 (second (assoc 'x ball-1))
     ;; 				 (second (assoc 'y ball-1))
     ;; 				 (second (assoc 'vx ball-1))
     ;; 				 (second (assoc 'vy ball-1))
     ;; 				 ))

      

      
     ;; ---------------- draw a grid say 150 x 150 -----------------------
      (draw-screen-grid cr)


     
      ;; -------------- end of drawing code -------------------------
      ;;(setq texture (sdl-createtexturefromsurface render cr))
      (setq texture (sdl-createtexturefromsurface render sdl-surface))
      
      ;; texture 
      
      ;; texture2 --- will that not just write over it ?
      (sdl-rendercopy render texture %null-ptr %null-ptr)

      ;; free the texture
      (sdl-destroy-texture texture)
      
      ;; show line
      (sdl-renderpresent render)

;;      );; -------------------------------------- only render if certain time has elapsed 

      
      
      ;; ?? 
      ;;(sdl-destroy-texture texture)
      
      ;; process events ...
      (catch 'poll-events
	(loop while t do
	  (let ((poll (sdl-pollevent ev-ptr)))
	    (cond
	      ((zerop poll) ;; when no more events to poll 
	       (throw 'poll-events t))
	      (t
	       ;; switch event type 
	       (let ((ev-type (cffi:mem-aref ev-ptr :int 0)))
		 ;;(format t "event type ~a ~%" ev-type)
		 (cond
		   ;; exit fast by throwing 
		   ((= ev-type +sdl-quit+)
		    ;;(format t "quitting !~%")
		    ;;(setq has-poll-event nil)
		    (setq *close* t)
		    (throw 'poll-events t))

		   
		   ((= ev-type +sdl-mousemotion+)
		    ;;(format t "mouse moving !~%")
		    
		    ;; read off respective indices manually from ev-ptr
		    (let ((x (cffi:mem-aref ev-ptr :int 5))
			  (y (cffi:mem-aref ev-ptr :int 6)))
		      ;;(format t "mouse motion v1.0 : mouse at position ~a ~a ~%" x y)
		      (setq *mouse-x* x)
		      (setq *mouse-y* y)
		      		   ;; how fast ?
		      (next-train-tick)

		      )

		    ;; can use structure definition and read it off that
		    (let ((type (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'type))
			  (timestamp (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'timestamp))
			  (window-id (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'window-id))
			  (which (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'which))
			  (state (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'state))
			  (x (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'x))
			  (y (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'y))
			  (xrel (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'xrel))
			  (yrel (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-motion-event) 'yrel)))
		      t
		      ;; (format t "mouse v2.0 [~a,~a,~a,~a,~a] : pos [~a,~a,~a,~a]~%"
		      ;; 	      type timestamp window-id which state x y xrel yrel)
		      
		      )
		    
		    );; --- mouse motion ----
		   
		   ((= ev-type +sdl-mousebuttondown+)
		    ;; (format t "mouse down !~%")		      
		    (let ((type (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'type))
			  (timestamp (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'timestamp))
			  (window-id (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'window-id))
			  (which (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'which))
			  (state (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'state))
			  (button (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'button))
			  (x (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'x))
			  (y (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'y))
			  (clicks (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'clicks))
			  )

		      t
		      
		      ;; (format t "mdown [~a,~a,~a,~a,~a] : pos [~a,~a,~a,~a]~%"
		      ;; 	      type timestamp window-id which state
		      ;; 	      (cond ((= button 1) "left")
		      ;; 		    ((= button 2) "middle")
		      ;; 		    ((= button 3) "right")
		      ;; 		    (t "none"))
		      ;; 	      x y clicks)
		      
		      )) ;; --- mouse down ---

		   
		   ((= ev-type +sdl-mousebuttonup+)
		    ;;(format t "mouse up !~%")		      
		    (let ((type (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'type))
			  (timestamp (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'timestamp))
			  (window-id (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'window-id))
			  (which (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'which))
			  (state (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'state))
			  (button (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'button))
			  (x (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'x))
			  (y (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'y))
			  (clicks (cffi:foreign-slot-value ev-ptr '(:struct sdl-mouse-button-event) 'clicks))
			  )

		      t
		      ;; (format t "mup [~a,~a,~a,~a,~a] : pos [~a,~a,~a,~a]~%"
		      ;; 	      type timestamp window-id which state
		      ;; 	      (cond ((= button 1) 'left)
		      ;; 		    ((= button 2) 'middle)
		      ;; 		    ((= button 3) 'right)
		      ;; 		    (t 'none))
		      ;; 	      x y clicks)

		      )) ;; --- mouse up ---

		   ((= ev-type +sdl-keyup+)
		    
		    ;; possible got whole thing topsy-turvey
		    (let (;;(type (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'type))
			  ;;(timestamp (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'timestamp))
			  ;;(window-id (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'window-id))
			  ;;(state (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'state))
			  ;;(repeat (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'repeat))
			  (scancode (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'scancode))
			  ;;(sym (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'sym))
			  (modifier (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'modifier)))

		      t
		      
		      (format t "key up : scancode ~a => ~a : modifier ~a => ~a ~%"
			      scancode
			      (decode-keyboard-scancode scancode)
			      modifier
			      (decode-keyboard-modifier modifier)
			      )

		      ;; report this key is now not pressed
		      (setf (aref *keys* scancode) nil)
		      
		      ;; if lift key up - do something ... land mine keys ?? 
		      

		      )) ;; ---- key up ------
		   
		   ((= ev-type +sdl-keydown+)

		    ;; possible got whole thing topsy-turvey
		    (let (;;(type (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'type))
			  ;;(timestamp (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'timestamp))
			  ;;(window-id (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'window-id))
			  ;;(state (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'state))
			  ;;(repeat (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'repeat))
			  (scancode (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'scancode))
			  ;;(sym (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'sym))
			  (modifier (cffi:foreign-slot-value ev-ptr '(:struct sdl-keyboard-event) 'modifier)))

		      t

		      (format t "key down : scancode ~a => ~a : modifier ~a => ~a~%"
			      scancode
			      (decode-keyboard-scancode scancode)
			      modifier
			      (decode-keyboard-modifier modifier)
			      )

		      ;; report this is now pressed  -without modifiers 
		      (setf (aref *keys* scancode) t)

		      
		      ;; alt keys do not seem to be detected correctly on cherry keyboard ...

		      ;; if press up arrow 
		      (when (= scancode +sdl-scancode-up+)
			(format t "action UP !~%")
			(setq *last-key-event-string* "UP-ARROW")

			(incf *grid-top-y* +1)

			)

		      ;; if press down arrow 
		      (when (= scancode +sdl-scancode-down+)
			(format t "action DOWN !~%")
			(setq *last-key-event-string* "DOWN-ARROW")

			(incf *grid-top-y* -1)

			
			)

		      ;; if press left arrow
		      (when (= scancode +sdl-scancode-left+)
			(format t "action LEFT !~%")
			(setq *last-key-event-string* "LEFT-ARROW")

			(incf *grid-top-x* +1)
			
			)

		      ;; if press right arrow
		      (when (= scancode +sdl-scancode-right+)
			(format t "action RIGHT !~%")
			(setq *last-key-event-string* "RIGHT ARROW")

			(incf *grid-top-x* -1)
			
			)

		      ;; next train tick 
		      (when (= scancode +sdl-scancode-m+)
			(format t "action NEXT TICK !~%")
			(setq *last-key-event-string* "NEXT TICK")
			(next-train-tick)
			)

		      ;; prev train tick 
		      (when (= scancode +sdl-scancode-n+)
			(format t "action PREV TICK !~%")
			(setq *last-key-event-string* "PREV TICK")
			(prev-train-tick)
			)


		      
		      ;; ENLARGE ........
		      (when (= scancode +sdl-scancode-equals+)
			(format t "action ENLARGE !~%")
			(setq *last-key-event-string* "enlarge !")
			(zoom-enlarge)
			)
		      

		      ;; SHRINKING......
		      (when (= scancode +sdl-scancode-minus+)
			(format t "action SHRINK !~%")
			(setq *last-key-event-string* "shrink !")
			(zoom-shrink)
			)
		      		      
		      
		      ;; if press escape key
		      (when (= scancode +sdl-scancode-escape+)
			(setq *close* t)
			(throw 'poll-events t))
		      
		      
		      )) ;; --- keydown ---

		   
		   )))))))))

  
  ;; roughly 1 60th of a second
  ;; (sdl-delay (floor (/ 1000 60)))
  ;;(sleep 3)
  
  ;;(sdl-destroytexture texture)
  (sdl-destroyrenderer render)
  (sdl-destroywindow window)

  (img-quit)
  (sdl-quit)
  )


;; -------- tell user how start trains demo
(format t "(cairo-sdl::trains-demo) to start the train simulation ~%")

;;(demo)


