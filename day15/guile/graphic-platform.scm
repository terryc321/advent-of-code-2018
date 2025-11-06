;; -*- geiser-scheme-implementation: guile -*-

;; set load path on starting
;; (add-to-load-path "/home/terry/code/advent-code/advent-of-code-2018/day15/guile/")
;; place libpixelformat.so in correct location -- see README.md
;; start emacs
;; guile cannot find shared libraries if guile already started
;; > (add-to-load-path "/home/terry/code/advent-code/advent-of-code-2018/day15/guile/")
;; compile this file using geiser
;; > (demo)
;; 

;; where are we located in the file system 
(define (base s)
  (string-append
   "/home/terry/code/advent-code/advent-of-code-2018/day15/guile/"
   s))

#|

game1.scm

can we get a sprite on to screen at x y ?
how do we do animation ?

can display image on screen
we can mask out a color of image
can we make a png from what is on the screen ?
e.g can we draw a car on screen using paint pots and then save that as the car image



|#




(use-modules (ice-9 optargs)) ;; optional args
(use-modules (system foreign)) ;; %null-pointer
(use-modules (system foreign-library))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 format))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print)) 
(define pp pretty-print)

;; these modules are not in guile ecosystem they require an altered %load-path to make them visible
(use-modules (macros fcase))
(use-modules (macros inc))
(use-modules ((graphics sdl2 sdl) #:prefix sdl:))
(use-modules ((graphics sdl2 image) #:prefix img:))
(use-modules ((graphics cairo cairo) #:prefix cairo:))


(define (setup) #f)

(define *keyboard-fn-vector* (make-vector 516 #f)) ;; somewhat largeer than 512

(define (register-keyboard-fn i fn)
  (vector-set! *keyboard-fn-vector* i fn))

;; if there is a procedure at index i in keyboard-fn-vector then call that procedure of no args
(define (call-keyboard-fn i)
  (let ((fn (vector-ref *keyboard-fn-vector* i)))
    (when (procedure? fn)
      (fn))))

(define (make-sdl-rect x y w h)
  (let* ((bv (make-bytevector 16 0)))
    (bytevector-s32-native-set! bv 0 x);; x
    (bytevector-s32-native-set! bv 4 y);; y
    (bytevector-s32-native-set! bv 8 w);; width
    (bytevector-s32-native-set! bv 12 h);; height
    bv))

(define (make-sdl-rect-pointer x y w h)
  (bytevector->pointer (make-sdl-rect x y w h)))

(define (make-sdl-color red green blue)  (logior (ash red 16) (ash green 8) blue))

(define *mouse-x* 0)
(define *mouse-y* 0)
;; (define *screen-width* 1920) ;; full screen my monitor
;; (define *screen-height* 1080)
(define *screen-width* 1024) ;; a window
(define *screen-height* 768)

(define *resized* #f)
(define *quit* #f)

;; event is a union struct 36 bytes 
(define *event* (make-bytevector 36 0))

(define *first-run* #t)

(define *event-counter* 0)
(define *window* #f)
(define *render* #f)
(define *surface* #f)
(define *cairo-surface* #f)
(define *cr* #f)
(define *texture* #f)

(define *sprites3-texture* #f)
(define *sprites3-width* 33)
(define *sprites3-height* 46)
(define *sprites3-number* 0)


(define *sprites-texture* #f)
(define *background-texture* #f)
(define *man-texture* #f)

(define *frames* 0)

(define *sprites-offset-x* 32)
(define *sprites-offset-y* 37)
(define *sprites-width* 40)
(define *sprites-height* 47)
(define *sprites-count-x* 20)
(define *sprites-count-y* 6)

(define *sprite-angle* 0.0)
(define *sprite-number* 0)
(define *sprite-number-a* 0)
(define *sprite-number-b* 0)

;; (map (lambda (y)
;;        (let ((xs (map (lambda (x) (+ 32 (* x *sprites-width*))) (iota 20))))
;; 	 (map (lambda (x) (list x y *sprites-width* *sprites-height*)) xs)))
;;      (map (lambda (y) (+ 37 (* y *sprites-height*))) (iota 5)))

;; (apply append
;;        (let ((high 5)(wide 5)(offset-x 0)(offset-y 0)(width 33)(height 46))
;; 	 (map (lambda (y)
;; 		(let ((xs (map (lambda (x) (+ offset-x (* x width))) (iota wide))))
;; 		  (map (lambda (x) (list x y width height)) xs)))
;; 	      (map (lambda (y) (+ offset-y (* y height))) (iota high)))))


;;(define *sprite3
(define *sprite3-number* 0)
(define *sprite3-vector*
  (let* ((w *sprites3-width*)(h *sprites3-height*)(h2 (* h 2)))
    (list->vector
     (list
      (make-sdl-rect-pointer (* w 0) h w h)
      (make-sdl-rect-pointer (* w 1) h w h)
      (make-sdl-rect-pointer (* 2 w) h w h)
      (make-sdl-rect-pointer (* 3 w) h w h)))))


(define *sprite4-number* 0)
(define *sprite4-vector*
  (let* ((w *sprites3-width*)(h *sprites3-height*)(h2 (* h 2)))
    (list->vector
     (list
      (make-sdl-rect-pointer (* w 0) 0 w h)
      (make-sdl-rect-pointer (* w 1) 0 w h)
      (make-sdl-rect-pointer (* 2 w) 0 w h)
      (make-sdl-rect-pointer (* 3 w) 0 w h)))))


(define *sprite5-number* 0)
(define *sprite5-vector*
  (let* ((w *sprites3-width*)(h *sprites3-height*)(h2 (* h 2)))
    (list->vector
     (list
      (make-sdl-rect-pointer (* w 0) h2 w h)
      (make-sdl-rect-pointer (* w 1) h2 w h)
      (make-sdl-rect-pointer (* 2 w) h2 w h)
      (make-sdl-rect-pointer (* 3 w) h2 w h)))))


(define *sprite6-number* 0)
(define *sprite6-vector*
  (let* ((w *sprites3-width*)(h *sprites3-height*)(h3 (* h 3)))
    (list->vector
     (list
      (make-sdl-rect-pointer (* w 0) h3 w h)
      (make-sdl-rect-pointer (* w 1) h3 w h)
      (make-sdl-rect-pointer (* 2 w) h3 w h)
      (make-sdl-rect-pointer (* 3 w) h3 w h)))))




(define *sprite-vector*
  (list->vector
   (apply append
	  (map (lambda (y)
		 (let ((xs (map (lambda (x) (+ 32 (* x *sprites-width*))) (iota 20))))
		   (map (lambda (x) (make-sdl-rect-pointer x y *sprites-width* *sprites-height*)) xs)))
	       (map (lambda (y) (+ 37 (* y *sprites-height*))) (iota 5))))))
   

(define (get-sprite)
  (let ((a *sprite-number-a*)
	(b *sprite-number-b*))
    (bytevector->pointer (make-sdl-rect (+ *sprites-offset-x* (* *sprites-width* a))
					(*sprites-offset-y* (* *sprites-height* b))
					*sprites-width*
					*sprites-height*))))

(define (next-sprite)
    (let ((a *sprite-number-a*)
	  (b *sprite-number-b*))
      ;; next sprite horizontally
      (set! a (+ a 1))
      (when (>= a *sprites-count-x*)
	(set! a 0)
	(set! b (+ b 1)))
      (when (>= b *sprites-count-y*)
	(set! b 0)
	(set! a 0))
      (when (and (= b 5) (> a 4))
	(set! a 0)
	(set! b 0))
      (set! *sprite-number-a* a)
      (set! *sprite-number-b* b)))


;; =================================
(define (main-loop)  
  ;; poll for an event
  (while (not (= 0 (sdl:poll-event (bytevector->pointer *event*))))
    ;; tot up each time we have an event
    (inc! *event-counter*)
    (let ((type (bytevector-u32-native-ref *event* 0)))
      (cond
       ((= type sdl:*quit*) ;; ======== quit event ==================
	(format #t "the user quit the application !~%")
	;; if we quit - set quit flag to true and exit
	(let ((type (bytevector-u32-native-ref *event* 0))
	      (timestamp (bytevector-u32-native-ref *event* 4)))
	  (set! *quit* #t))) ;; end quit event	 
       ((= type sdl:*window-event*) ;; ======== window event ==================
	(let ((type (bytevector-u32-native-ref *event* 0))
	      (timestamp (bytevector-u32-native-ref *event* 4))
	      (windowid (bytevector-u32-native-ref *event* 8))
	      (event (bytevector-u8-ref *event* 12))
	      (padding1 (bytevector-u8-ref *event* 13))
	      (padding2 (bytevector-u8-ref *event* 14))
	      (padding3 (bytevector-u8-ref *event* 15 ))
	      (data1 (bytevector-s32-native-ref *event* 16 ))
	      (data2 (bytevector-s32-native-ref *event* 20 )))
	  ;; window events are difficult to distinguish so we gave all events polled an event counter
	  (fcase event
		 ((sdl:*window-event-none*) (format #t "window event ~a none~%" *event-counter*))
		 ((sdl:*window-event-shown*) (format #t "window event ~a shown ~%" *event-counter*))
		 ((sdl:*window-event-hidden*) (format #t "window event ~a hidden ~%" *event-counter*))
		 ((sdl:*window-event-moved*) (format #t "window event ~a moved ~a ~a~%" *event-counter* data1 data2))
		 ((sdl:*window-event-exposed*) (format #t "window event ~a exposed~%" *event-counter* ))
		 ((sdl:*window-event-resized*)
		  (handle-window-resize-event data1 data2)
		  (format #t "window event ~a resized w = ~a / h = ~a~%" *event-counter* data1 data2))
		 ((sdl:*window-event-changed*) (format #t "window event ~a changed~%" *event-counter* ))
		 ((sdl:*window-event-minimized*) (format #t "window event ~a minimized~%" *event-counter* ))
		 ((sdl:*window-event-maximized*) (format #t "window event ~a maximized~%" *event-counter* ))
		 ((sdl:*window-event-restored*) (format #t "window event ~a restored~%" *event-counter* ))
		 ((sdl:*window-event-enter*) (format #t "window event ~a enter~%" *event-counter* ))
		 ((sdl:*window-event-leave*) (format #t "window event ~a leave~%" *event-counter* ))
		 ((sdl:*window-event-focus-gained*) (format #t "window event ~a focus gained~% " *event-counter* ))
		 ((sdl:*window-event-focus-lost*) (format #t "window event ~a focus lost~%" *event-counter* ))
		 ((sdl:*window-event-close*) (format #t "window event ~a close~%" *event-counter* ))
		 ((sdl:*window-event-take-focus*) (format #t "window event ~a take focus~%" *event-counter* ))
		 ((sdl:*window-event-hit-test*) (format #t "window event ~a hit test~%" *event-counter* ))
		 ((sdl:*window-event-icc-prof-changed*) (format #t "window event ~a icc prof changed ~%" *event-counter* ))
		 ((sdl:*window-event-display-changed*) (format #t "window event ~a display changed~%" *event-counter* ))
		 (else (format #t "window event ~a something else happended ~%" *event-counter*)))
	  #t)) ;; ==== end of window event ===

       ((= type sdl:*keydown*) ;; ======== keydown event ==================
	(let ((type (bytevector-u32-native-ref *event* 0))
	      (timestamp (bytevector-u32-native-ref *event* 4))
	      (windowid (bytevector-u32-native-ref *event* 8))
	      (state (bytevector-u8-ref *event* 12))
	      (repeat (bytevector-u8-ref *event* 13))
	      (padding2 (bytevector-u8-ref *event* 14))
	      (padding3 (bytevector-u8-ref *event* 15 ))
	      (keysym-scancode (bytevector-u32-native-ref *event* 16 ))
	      (keysym-sym (bytevector-s32-native-ref *event* 20 ))
	      (keysym-mod (bytevector-u16-native-ref *event* 24 )))

	  ;; do something first time this is run
	  (when *first-run*
	    ;; allows us to escape the gui event loop easily
	    (register-keyboard-fn sdl:*scancode-escape*
				  (lambda () (set! *quit* #t)(format #t "user quit ! ~%")))
	    (register-keyboard-fn sdl:*scancode-n*
				  (lambda () (inc! *sprite-number*)))
	    
	    (set! *first-run* #f))
	  
	  ;; call relevant key handler
	  (call-keyboard-fn keysym-scancode))) ;; end key down
       ((= type sdl:*keyup*) ;; ======== keyup event ==================
	;;(format #t "the user released a key !~%")
	#f) ;; end key up 
       ((= type sdl:*mousemotion*) ;; ======== mouse motion event ==================
	(let ((type (bytevector-u32-native-ref *event* 0 ))
	      (timestamp (bytevector-u32-native-ref *event* 4 ))
	      (windowid (bytevector-u32-native-ref *event* 8 ))
	      (state (bytevector-u32-native-ref *event* 12 ))
	      (x (bytevector-s32-native-ref *event* 20 ))
	      (y (bytevector-s32-native-ref *event* 24 ))
	      (xrel (bytevector-s32-native-ref *event* 28 ))
	      (yrel (bytevector-s32-native-ref *event* 32 )))
	  ;; (format #t "mouse move (~a ~a ~a ~a " type timestamp windowid state)
	  ;; (format #t " (pos:~a ~a) (rel:~a ~a) ~%" x y xrel yrel)
	  (set! *mouse-x* x)
	  (set! *mouse-y* y)))
       (#t #f))))

  (draw-frame))

;; ===============================
(define (handle-window-resize-event w h)
  (set! *screen-width* w)
  (set! *screen-height* h)
  (set! *resized* #t))
  



;; =================================

(define (compensate-window-resize-with-desired-aspect desired-aspect)
  ;; is this not the default ?
  (cairo:identity-matrix *cr*)
  
  (let ((current-aspect (/ *screen-width* *screen-height*))
	(draw-width 0)
	(draw-height 0)
	(offset-x 0)
	(offset-y 0))

    (cond
     ((> current-aspect desired-aspect)
      ;;window too wide
      (set! draw-height *screen-height*)
      (set! draw-width (/ *screen-height* desired-aspect))
      (set! offset-x (/ 2 (- *screen-width* draw-width))))
     (#t ;; window too tall
      (set! draw-width *screen-width*)
      (set! draw-height (/ *screen-width* desired-aspect))
      (set! offset-y (/ 2 (- *screen-height* draw-height)))))

    (cairo:translate *cr* offset-x offset-y)
    (cairo:scale *cr* draw-width draw-height)

    ;; now draw in normalized space
    ))

;; ==========================

(define (draw-cairo-specific)
  (cairo:set-source-rgba *cr* 1 1 1 1)
  (cairo:rectangle *cr* 0 0 *screen-width* *screen-height*)
  (cairo:fill *cr*)
  
  ;; (define xc 320.0)
  ;; (define yc 240.0)
  (define xc *mouse-x*)
  (define yc *mouse-y*)
  (define radius 200.0) ;; was 200.0
  (define pi (acos -1)) ;; 3.1415926535898.. ish 
  (define angle1 (* 45 (/ pi 180)))
  (define angle2 (* 180 (/ pi 180)))

  (cairo:set-source-rgba *cr* 0 0 0 1)
  (cairo:set-line-width *cr* 10)
  (cairo:arc *cr* xc yc radius angle1 angle2)
  (cairo:stroke *cr*)
  (cairo:set-source-rgba *cr* 1 0.2 0.2 0.6)    
  (cairo:set-line-width *cr* 6.0)
  (cairo:arc *cr* xc yc 10.0 0 (* 2 pi))
  (cairo:fill *cr*) 
  (cairo:arc *cr*  xc  yc  radius  angle1  angle1) 
  (cairo:line-to *cr*  xc  yc) 
  (cairo:arc *cr*  xc  yc  radius  angle2  angle2) 
  (cairo:line-to *cr*  xc  yc) 
  (cairo:stroke *cr*) 
  )

(define (draw-cairo)

  ;; compensate resize 
  (when *resized*
    ;; ?? resize compensate-window-resize-with-desired-aspect does not work ?? 
    ;;(compensate-window-resize-with-desired-aspect (/ 16 9))
    (set! *resized* #f))

  (draw-cairo-specific)
  
  (cairo:surface-flush *cairo-surface*)
  (sdl:update-texture *texture* %null-pointer (sdl:surface-pixels *surface*) (sdl:surface-pitch *surface*))
  (sdl:render-copy *render* *texture* %null-pointer %null-pointer)
  )


(define (sprite-3 i x y)  
  (sdl:render-copy *render*
		   *sprites3-texture*
		   ;; src
		   (make-sdl-rect-pointer (* i 33)  
					  0
					  *sprites3-width*
					  *sprites3-height*)
		   ;; dest
		   (make-sdl-rect-pointer x
					  y
					  (* 4 *sprites3-width*)
					  (* 4 *sprites3-height*))))
  



(define (draw-sdl)
   ;; red square
  (let ((bv (make-bytevector (* 4 (sdl:size-int)) 0)))
    (bytevector-s32-native-set! bv 0 *mouse-x*)
    (bytevector-s32-native-set! bv 4 *mouse-y*)
    (bytevector-s32-native-set! bv 8 50)
    (bytevector-s32-native-set! bv 12 50)
    (sdl:set-render-draw-color *render* 0 0 255 255)
    (sdl:render-fill-rect *render* (bytevector->pointer bv)))

  
  ;; background
  (sdl:render-copy *render* *background-texture* %null-pointer %null-pointer)

  ;; ;; man texture
  ;; (let ((src %null-pointer)
  ;; 	(dest   (let ((bv (make-bytevector 16 0))) ;; 16 = 4 ints , each size 4
  ;; 		  (bytevector-s32-native-set! bv 0 *mouse-x*)
  ;; 		(bytevector-s32-native-set! bv 4  *mouse-y*)
  ;; 		(bytevector-s32-native-set! bv 8  64) ;; man is 64 x 126 pixels
  ;; 		(bytevector-s32-native-set! bv 12 126)
  ;; 		(bytevector->pointer bv))))
  ;;   (sdl:render-copy *render* *man-texture* %null-pointer dest)
  ;;   )

  ;; (sprite-3 0 0 0)  
  ;; (sprite-3 1 (* 4 *sprites3-width*) 0) ;; width sprite also scaling of sprite
  ;; (sprite-3 2 (* 2(* 4 *sprites3-width*)) 0) ;; width sprite also scaling of sprite
  ;; (sprite-3 3 (* 3(* 4 *sprites3-width*)) 0) ;; width sprite also scaling of sprite
  ;; (sprite-3 4 (* 4(* 4 *sprites3-width*)) 0) ;; width sprite also scaling of sprite
  ;; (sprite-3 5 (* 5(* 4 *sprites3-width*)) 0) ;; width sprite also scaling of sprite
  
  ;; (sdl:render-copy *render*
  ;; 		   *sprites3-texture*
  ;; 		   ;; src
  ;; 		   (make-sdl-rect-pointer 0  
  ;; 					  0
  ;; 					  *sprites3-width*
  ;; 					  *sprites3-height*)
  ;; 		   ;; dest
  ;; 		   (make-sdl-rect-pointer 0
  ;; 					  0
  ;; 					  (* 4 *sprites3-width*)
  ;; 					  (* 4 *sprites3-height*)))
  
				   
  
  #|
  (map (lambda (x)
	 (map (lambda (y)
		(let ((xx (* 2 x *sprites3-width*))
		      (yy (* 2 y *sprites3-height*)))
		  (sdl:render-copy *render*
				   *sprites3-texture*				   
				   (let ((i *sprites3-number*)) ;; where in sprite map image to get sprite
				     (cond
				      ((and (>= i 0)(< i (vector-length *sprite3-vector*))) (vector-ref *sprite3-vector* i))
				      (#t (set! *sprites3-number* 0)
					  (vector-ref *sprites3-vector* 0))))
				   ;; (make-sdl-rect-pointer 32
				   ;; 			  37
				   ;; 			  *sprites-width*
				   ;; 			  *sprites-height*)
				   (make-sdl-rect-pointer xx ;;*mouse-x* ;; where to put sprite??
							  yy ;; *mouse-y*
							  (* 6 *sprites3-width*) ;; magnify it
							  (* 6 *sprites3-height*)))))

		  
		  (iota 10)))
       (iota 5))
  |#
	 

  (sdl:render-copy *render*
		   *sprites3-texture*
		   (let ((i *sprite3-number*)) ;; where in sprite map image to get sprite
		     (cond
		      ((and (>= i 0)(< i (vector-length *sprite3-vector*))) (vector-ref *sprite3-vector* i))
		      (#t (set! *sprite3-number* 0)
			  (vector-ref *sprite3-vector* 0))))
		   (make-sdl-rect-pointer 0 ;; where to put sprite
					  0
					  (* 4 *sprites3-width*) ;; magnify it
					  (* 4 *sprites3-height*)))
  
  (sdl:render-copy *render*
		   *sprites3-texture*
		   (let ((i *sprite4-number*)) ;; where in sprite map image to get sprite
		     (cond
		      ((and (>= i 0)(< i (vector-length *sprite4-vector*))) (vector-ref *sprite4-vector* i))
		      (#t (set! *sprite4-number* 0)
			  (vector-ref *sprite4-vector* 0))))
		   (make-sdl-rect-pointer (* 1 4 *sprites3-width*) ;; where to put sprite
					  0
					  (* 4 *sprites3-width*) ;; magnify it
					  (* 4 *sprites3-height*)))
  
	 
  (sdl:render-copy *render*
		   *sprites3-texture*
		   (let ((i *sprite5-number*)) ;; where in sprite map image to get sprite
		     (cond
		      ((and (>= i 0)(< i (vector-length *sprite5-vector*))) (vector-ref *sprite5-vector* i))
		      (#t (set! *sprite5-number* 0)
			  (vector-ref *sprite5-vector* 0))))
		   (make-sdl-rect-pointer (* 2 4 *sprites3-width*) ;; where to put sprite
					  0
					  (* 4 *sprites3-width*) ;; magnify it
					  (* 4 *sprites3-height*)))
  
  (sdl:render-copy *render*
		   *sprites3-texture*
		   (let ((i *sprite6-number*)) ;; where in sprite map image to get sprite
		     (cond
		      ((and (>= i 0)(< i (vector-length *sprite6-vector*))) (vector-ref *sprite6-vector* i))
		      (#t (set! *sprite6-number* 0)
			  (vector-ref *sprite6-vector* 0))))
		   (make-sdl-rect-pointer (* 3 4 *sprites3-width*) ;; where to put sprite
					  0
					  (* 4 *sprites3-width*) ;; magnify it
					  (* 4 *sprites3-height*)))
  

  
  ;; sprites
  ;;(sdl:render-copy *render* *sprites-texture* %null-pointer %null-pointer)
  (sdl:render-copy-ex *render*
		      *sprites-texture*
		      (let ((i *sprite-number*)) ;; where in sprite map image to get sprite
			(cond
			 ((and (>= i 0)(< i (vector-length *sprite-vector*))) (vector-ref *sprite-vector* i))
			 (#t (set! *sprite-number* 0)
			     (vector-ref *sprite-vector* 0))))
		      (make-sdl-rect-pointer *mouse-x* ;; where to put sprite
					     *mouse-y*
					     (* 6 *sprites-width*) ;; magnify it
					     (* 6 *sprites-height*))
		      *sprite-angle*
		      %null-pointer
		      sdl:*flip-none*)

  
  
  ;; next sprite
  (inc! *frames*)
  (when (> *frames* 15)
    (inc! *sprite-angle* 5)
    (when (> *sprite-angle* 360.0)
      (set! *sprite-angle* 0.0))
    (inc! *sprite-number*)
    (inc! *sprite3-number*)    
    (inc! *sprite4-number*)
    (inc! *sprite5-number*)
    (inc! *sprite6-number*)    
    (set! *frames* 0))
  
  )





(define (draw-frame)
  (draw-cairo)
  ;;(draw-sdl)
  (sdl:render-present *render*))


;;cairo:*operator-source*


;; ================



(define (demo)
  (set! *quit* #f)
  (set! *first-run* #t)  

  ;; change to correct directory for demo to work
  ;;(chdir "/home/terry/code/guile-scheme/guile-sdl2-cairo/guile/demos")

  (sdl:init sdl:*init-video*)

  (sdl:show-cursor 0)
  
  ;; cannot pull documentation out when using sdl-poll-event
  ;; sdl img init
  (img:init (logior 
		     img:*init-jpg*
		     img:*init-png*))
  
  ;;(img-init ) ;;
  ;; sdl ttf init  
  (set! *window* (sdl:create-window (string->pointer "cairo demonstration window 0.1a")
				    0
				    0
				    *screen-width*
				    *screen-height*
				    (logior
				     sdl:*window-allow-highdpi*
				     sdl:*window-always-on-top*
				     ;;sdl:*window-fullscreen*
				     )))
;; 			
;; ;; convenience middle of my screen 1920 x 1080 default resolution
;; (define (create-window title width height)
;;   (let ((x (floor (/ (- 1920 width) 2)))
;; 	(y (floor (/ (- 1080 height) 2)))
;; 	(flags (logior ;;*window-fullscreen-desktop*
;; 		        *window-resizeable*
;; 		        *window-allow-highdpi*
;; 			*window-always-on-top*
;; 			;;*window-fullscreen*
;; 		       ;;*window-borderless*
;; 		       *window-shown*)))
;;     (%create-window (string->pointer title) x y width height flags)))
  
  
  (set! *render* (sdl:create-renderer *window* -1 
				       (logior sdl:*renderer-accelerated*  sdl:*renderer-present-vsync*)))

  ;;  (set! *surface* (create-rgb-surface 0 *screen-width* *screen-height*  ))
  (set! *surface* (sdl:create-rgb-surface 0 *screen-width* *screen-height* 32
					  #x00FF0000 #x0000FF00 #x000000FF #xFF000000))

  
  (set! *cairo-surface* (cairo:image-surface-create-for-data (sdl:surface-pixels *surface*)
							     cairo:*format-argb32*
							     *screen-width*
							     *screen-height*
							     (sdl:surface-pitch *surface*)))
  (set! *cr* (cairo:create *cairo-surface*))
  (set! *texture* (sdl:create-texture *render*
				      sdl:*pixelformat-argb8888*
				      sdl:*texture-streaming*
				      *screen-width*
				      *screen-height*))

  
  ;; sprites
  (let ((surf (img:load (string->pointer (base "./images/sprites2.png")))))
    ;; mask for sprites is fully red R gb
    (sdl:set-color-key surf #x1 (sdl:map-rgb (sdl:surface-pixelformat surf) #xFF #x00 #x00)) 
    (set! *sprites-texture* (sdl:create-texture-from-surface *render* surf))
    (format #t "sprites texture ~a~%" *sprites-texture*)
    (sdl:free-surface surf))

  ;; sprites3b
  (let ((surf (img:load (string->pointer (base "./images/sprites3b.png")))))
    ;; mask for sprites is fully red R gb
    (sdl:set-color-key surf #x1 (sdl:map-rgb (sdl:surface-pixelformat surf) #x78 #x00 #x00)) 
    (set! *sprites3-texture* (sdl:create-texture-from-surface *render* surf))
    (format #t "sprites texture ~a~%" *sprites3-texture*)
    (sdl:free-surface surf))

  
  
  ;; background
  (let ((surf (img:load (string->pointer (base "images/background2.png")))))
    (set! *background-texture* (sdl:create-texture-from-surface *render* surf))
    (format #t "background texture ~a~%" *background-texture*)
    (sdl:free-surface surf))
  
  ;; man 
  (let ((surf (img:load (string->pointer (base "./images/man.png")))))
    (format #t "surf ~a~%" surf)
    (sdl:set-color-key surf #x1 (sdl:map-rgb (sdl:surface-pixelformat surf) #x0 #xFF #xFF))
    (set! *man-texture* (sdl:create-texture-from-surface *render* surf))
    (format #t "man texture ~a~%" *man-texture*)
    (sdl:free-surface surf))
  
  
  (register-keys)
  
  (while (not *quit*)
    (main-loop))
  (cleanup))


;;=======================



;;
;; usage
;; load using correct environment and %load-path
;; > (demo)
;;
;; should see a nice pink square top right and sort of arc
;;

(define (register-keys)
  "simply assigns a routine that prints that key scancode to console "
  
  (register-keyboard-fn sdl:*scancode-unknown* (lambda () (format #t "user pressed sdl-scancode-unknown key ~%")))
  (register-keyboard-fn sdl:*scancode-a* (lambda () (format #t "user pressed sdl-scancode-a key ~%")))
  (register-keyboard-fn sdl:*scancode-b* (lambda () (format #t "user pressed sdl-scancode-b key ~%")))
  (register-keyboard-fn sdl:*scancode-c* (lambda () (format #t "user pressed sdl-scancode-c key ~%")))
  (register-keyboard-fn sdl:*scancode-d* (lambda () (format #t "user pressed sdl-scancode-d key ~%")))
  (register-keyboard-fn sdl:*scancode-e* (lambda () (format #t "user pressed sdl-scancode-e key ~%")))
  (register-keyboard-fn sdl:*scancode-f* (lambda () (format #t "user pressed sdl-scancode-f key ~%")))
  (register-keyboard-fn sdl:*scancode-g* (lambda () (format #t "user pressed sdl-scancode-g key ~%")))
  (register-keyboard-fn sdl:*scancode-h* (lambda () (format #t "user pressed sdl-scancode-h key ~%")))
  (register-keyboard-fn sdl:*scancode-i* (lambda () (format #t "user pressed sdl-scancode-i key ~%")))
  (register-keyboard-fn sdl:*scancode-j* (lambda () (format #t "user pressed sdl-scancode-j key ~%")))
  (register-keyboard-fn sdl:*scancode-k* (lambda () (format #t "user pressed sdl-scancode-k key ~%")))
  (register-keyboard-fn sdl:*scancode-l* (lambda () (format #t "user pressed sdl-scancode-l key ~%")))
  (register-keyboard-fn sdl:*scancode-m* (lambda () (format #t "user pressed sdl-scancode-m key ~%")))
  (register-keyboard-fn sdl:*scancode-n* (lambda () (format #t "user pressed sdl-scancode-n key ~%")))
  (register-keyboard-fn sdl:*scancode-o* (lambda () (format #t "user pressed sdl-scancode-o key ~%")))
  (register-keyboard-fn sdl:*scancode-p* (lambda () (format #t "user pressed sdl-scancode-p key ~%")))
  (register-keyboard-fn sdl:*scancode-q* (lambda () (format #t "user pressed sdl-scancode-q key ~%")))
  (register-keyboard-fn sdl:*scancode-r* (lambda () (format #t "user pressed sdl-scancode-r key ~%")))
  (register-keyboard-fn sdl:*scancode-s* (lambda () (format #t "user pressed sdl-scancode-s key ~%")))
  (register-keyboard-fn sdl:*scancode-t* (lambda () (format #t "user pressed sdl-scancode-t key ~%")))
  (register-keyboard-fn sdl:*scancode-u* (lambda () (format #t "user pressed sdl-scancode-u key ~%")))
  (register-keyboard-fn sdl:*scancode-v* (lambda () (format #t "user pressed sdl-scancode-v key ~%")))
  (register-keyboard-fn sdl:*scancode-w* (lambda () (format #t "user pressed sdl-scancode-w key ~%")))
  (register-keyboard-fn sdl:*scancode-x* (lambda () (format #t "user pressed sdl-scancode-x key ~%")))
  (register-keyboard-fn sdl:*scancode-y* (lambda () (format #t "user pressed sdl-scancode-y key ~%")))
  (register-keyboard-fn sdl:*scancode-z* (lambda () (format #t "user pressed sdl-scancode-z key ~%")))
  (register-keyboard-fn sdl:*scancode-1* (lambda () (format #t "user pressed sdl-scancode-1 key ~%")))
  (register-keyboard-fn sdl:*scancode-2* (lambda () (format #t "user pressed sdl-scancode-2 key ~%")))
  (register-keyboard-fn sdl:*scancode-3* (lambda () (format #t "user pressed sdl-scancode-3 key ~%")))
  (register-keyboard-fn sdl:*scancode-4* (lambda () (format #t "user pressed sdl-scancode-4 key ~%")))
  (register-keyboard-fn sdl:*scancode-5* (lambda () (format #t "user pressed sdl-scancode-5 key ~%")))
  (register-keyboard-fn sdl:*scancode-6* (lambda () (format #t "user pressed sdl-scancode-6 key ~%")))
  (register-keyboard-fn sdl:*scancode-7* (lambda () (format #t "user pressed sdl-scancode-7 key ~%")))
  (register-keyboard-fn sdl:*scancode-8* (lambda () (format #t "user pressed sdl-scancode-8 key ~%")))
  (register-keyboard-fn sdl:*scancode-9* (lambda () (format #t "user pressed sdl-scancode-9 key ~%")))
  (register-keyboard-fn sdl:*scancode-0* (lambda () (format #t "user pressed sdl-scancode-0 key ~%")))
  (register-keyboard-fn sdl:*scancode-return* (lambda () (format #t "user pressed sdl-scancode-return key ~%")))
  (register-keyboard-fn sdl:*scancode-escape* (lambda () (format #t "user pressed sdl-scancode-escape key ~%")))
  (register-keyboard-fn sdl:*scancode-backspace* (lambda () (format #t "user pressed sdl-scancode-backspace key ~%")))
  (register-keyboard-fn sdl:*scancode-tab* (lambda () (format #t "user pressed sdl-scancode-tab key ~%")))
  (register-keyboard-fn sdl:*scancode-space* (lambda () (format #t "user pressed sdl-scancode-space key ~%")))
  (register-keyboard-fn sdl:*scancode-minus* (lambda () (format #t "user pressed sdl-scancode-minus key ~%")))
  (register-keyboard-fn sdl:*scancode-equals* (lambda () (format #t "user pressed sdl-scancode-equals key ~%")))
  (register-keyboard-fn sdl:*scancode-leftbracket* (lambda () (format #t "user pressed sdl-scancode-leftbracket key ~%")))
  (register-keyboard-fn sdl:*scancode-rightbracket* (lambda () (format #t "user pressed sdl-scancode-rightbracket key ~%")))
  (register-keyboard-fn sdl:*scancode-backslash* (lambda () (format #t "user pressed sdl-scancode-backslash key ~%")))
  (register-keyboard-fn sdl:*scancode-nonushash* (lambda () (format #t "user pressed sdl-scancode-nonushash key ~%")))
  (register-keyboard-fn sdl:*scancode-semicolon* (lambda () (format #t "user pressed sdl-scancode-semicolon key ~%")))
  (register-keyboard-fn sdl:*scancode-apostrophe* (lambda () (format #t "user pressed sdl-scancode-apostrophe key ~%")))
  (register-keyboard-fn sdl:*scancode-grave* (lambda () (format #t "user pressed sdl-scancode-grave key ~%")))
  (register-keyboard-fn sdl:*scancode-comma* (lambda () (format #t "user pressed sdl-scancode-comma key ~%")))
  (register-keyboard-fn sdl:*scancode-period* (lambda () (format #t "user pressed sdl-scancode-period key ~%")))
  (register-keyboard-fn sdl:*scancode-slash* (lambda () (format #t "user pressed sdl-scancode-slash key ~%")))
  (register-keyboard-fn sdl:*scancode-capslock* (lambda () (format #t "user pressed sdl-scancode-capslock key ~%")))
  (register-keyboard-fn sdl:*scancode-f1* (lambda () (format #t "user pressed sdl-scancode-f1 key ~%")))
  (register-keyboard-fn sdl:*scancode-f2* (lambda () (format #t "user pressed sdl-scancode-f2 key ~%")))
  (register-keyboard-fn sdl:*scancode-f3* (lambda () (format #t "user pressed sdl-scancode-f3 key ~%")))
  (register-keyboard-fn sdl:*scancode-f4* (lambda () (format #t "user pressed sdl-scancode-f4 key ~%")))
  (register-keyboard-fn sdl:*scancode-f5* (lambda () (format #t "user pressed sdl-scancode-f5 key ~%")))
  (register-keyboard-fn sdl:*scancode-f6* (lambda () (format #t "user pressed sdl-scancode-f6 key ~%")))
  (register-keyboard-fn sdl:*scancode-f7* (lambda () (format #t "user pressed sdl-scancode-f7 key ~%")))
  (register-keyboard-fn sdl:*scancode-f8* (lambda () (format #t "user pressed sdl-scancode-f8 key ~%")))
  (register-keyboard-fn sdl:*scancode-f9* (lambda () (format #t "user pressed sdl-scancode-f9 key ~%")))
  (register-keyboard-fn sdl:*scancode-f10* (lambda () (format #t "user pressed sdl-scancode-f10 key ~%")))
  (register-keyboard-fn sdl:*scancode-f11* (lambda () (format #t "user pressed sdl-scancode-f11 key ~%")))
  (register-keyboard-fn sdl:*scancode-f12* (lambda () (format #t "user pressed sdl-scancode-f12 key ~%")))
  (register-keyboard-fn sdl:*scancode-printscreen* (lambda () (format #t "user pressed sdl-scancode-printscreen key ~%")))
  (register-keyboard-fn sdl:*scancode-scrolllock* (lambda () (format #t "user pressed sdl-scancode-scrolllock key ~%")))
  (register-keyboard-fn sdl:*scancode-pause* (lambda () (format #t "user pressed sdl-scancode-pause key ~%")))
  (register-keyboard-fn sdl:*scancode-insert* (lambda () (format #t "user pressed sdl-scancode-insert key ~%")))
  (register-keyboard-fn sdl:*scancode-home* (lambda () (format #t "user pressed sdl-scancode-home key ~%")))
  (register-keyboard-fn sdl:*scancode-pageup* (lambda () (format #t "user pressed sdl-scancode-pageup key ~%")))
  (register-keyboard-fn sdl:*scancode-delete* (lambda () (format #t "user pressed sdl-scancode-delete key ~%")))
  (register-keyboard-fn sdl:*scancode-end* (lambda () (format #t "user pressed sdl-scancode-end key ~%")))
  (register-keyboard-fn sdl:*scancode-pagedown* (lambda () (format #t "user pressed sdl-scancode-pagedown key ~%")))
  (register-keyboard-fn sdl:*scancode-right* (lambda () (format #t "user pressed sdl-scancode-right key ~%")))
  (register-keyboard-fn sdl:*scancode-left* (lambda () (format #t "user pressed sdl-scancode-left key ~%")))
  (register-keyboard-fn sdl:*scancode-down* (lambda () (format #t "user pressed sdl-scancode-down key ~%")))
  (register-keyboard-fn sdl:*scancode-up* (lambda () (format #t "user pressed sdl-scancode-up key ~%")))
  (register-keyboard-fn sdl:*scancode-numlockclear* (lambda () (format #t "user pressed sdl-scancode-numlockclear key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-divide* (lambda () (format #t "user pressed sdl-scancode-kp-divide key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-multiply* (lambda () (format #t "user pressed sdl-scancode-kp-multiply key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-minus* (lambda () (format #t "user pressed sdl-scancode-kp-minus key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-plus* (lambda () (format #t "user pressed sdl-scancode-kp-plus key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-enter* (lambda () (format #t "user pressed sdl-scancode-kp-enter key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-1* (lambda () (format #t "user pressed sdl-scancode-kp-1 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-2* (lambda () (format #t "user pressed sdl-scancode-kp-2 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-3* (lambda () (format #t "user pressed sdl-scancode-kp-3 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-4* (lambda () (format #t "user pressed sdl-scancode-kp-4 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-5* (lambda () (format #t "user pressed sdl-scancode-kp-5 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-6* (lambda () (format #t "user pressed sdl-scancode-kp-6 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-7* (lambda () (format #t "user pressed sdl-scancode-kp-7 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-8* (lambda () (format #t "user pressed sdl-scancode-kp-8 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-9* (lambda () (format #t "user pressed sdl-scancode-kp-9 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-0* (lambda () (format #t "user pressed sdl-scancode-kp-0 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-period* (lambda () (format #t "user pressed sdl-scancode-kp-period key ~%")))
  (register-keyboard-fn sdl:*scancode-nonusbackslash* (lambda () (format #t "user pressed sdl-scancode-nonusbackslash key ~%")))
  (register-keyboard-fn sdl:*scancode-application* (lambda () (format #t "user pressed sdl-scancode-application key ~%")))
  (register-keyboard-fn sdl:*scancode-power* (lambda () (format #t "user pressed sdl-scancode-power key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-equals* (lambda () (format #t "user pressed sdl-scancode-kp-equals key ~%")))
  (register-keyboard-fn sdl:*scancode-f13* (lambda () (format #t "user pressed sdl-scancode-f13 key ~%")))
  (register-keyboard-fn sdl:*scancode-f14* (lambda () (format #t "user pressed sdl-scancode-f14 key ~%")))
  (register-keyboard-fn sdl:*scancode-f15* (lambda () (format #t "user pressed sdl-scancode-f15 key ~%")))
  (register-keyboard-fn sdl:*scancode-f16* (lambda () (format #t "user pressed sdl-scancode-f16 key ~%")))
  (register-keyboard-fn sdl:*scancode-f17* (lambda () (format #t "user pressed sdl-scancode-f17 key ~%")))
  (register-keyboard-fn sdl:*scancode-f18* (lambda () (format #t "user pressed sdl-scancode-f18 key ~%")))
  (register-keyboard-fn sdl:*scancode-f19* (lambda () (format #t "user pressed sdl-scancode-f19 key ~%")))
  (register-keyboard-fn sdl:*scancode-f20* (lambda () (format #t "user pressed sdl-scancode-f20 key ~%")))
  (register-keyboard-fn sdl:*scancode-f21* (lambda () (format #t "user pressed sdl-scancode-f21 key ~%")))
  (register-keyboard-fn sdl:*scancode-f22* (lambda () (format #t "user pressed sdl-scancode-f22 key ~%")))
  (register-keyboard-fn sdl:*scancode-f23* (lambda () (format #t "user pressed sdl-scancode-f23 key ~%")))
  (register-keyboard-fn sdl:*scancode-f24* (lambda () (format #t "user pressed sdl-scancode-f24 key ~%")))
  (register-keyboard-fn sdl:*scancode-execute* (lambda () (format #t "user pressed sdl-scancode-execute key ~%")))
  (register-keyboard-fn sdl:*scancode-help* (lambda () (format #t "user pressed sdl-scancode-help key ~%")))
  (register-keyboard-fn sdl:*scancode-menu* (lambda () (format #t "user pressed sdl-scancode-menu key ~%")))
  (register-keyboard-fn sdl:*scancode-select* (lambda () (format #t "user pressed sdl-scancode-select key ~%")))
  (register-keyboard-fn sdl:*scancode-stop* (lambda () (format #t "user pressed sdl-scancode-stop key ~%")))
  (register-keyboard-fn sdl:*scancode-again* (lambda () (format #t "user pressed sdl-scancode-again key ~%")))
  (register-keyboard-fn sdl:*scancode-undo* (lambda () (format #t "user pressed sdl-scancode-undo key ~%")))
  (register-keyboard-fn sdl:*scancode-cut* (lambda () (format #t "user pressed sdl-scancode-cut key ~%")))
  (register-keyboard-fn sdl:*scancode-copy* (lambda () (format #t "user pressed sdl-scancode-copy key ~%")))
  (register-keyboard-fn sdl:*scancode-paste* (lambda () (format #t "user pressed sdl-scancode-paste key ~%")))
  (register-keyboard-fn sdl:*scancode-find* (lambda () (format #t "user pressed sdl-scancode-find key ~%")))
  (register-keyboard-fn sdl:*scancode-mute* (lambda () (format #t "user pressed sdl-scancode-mute key ~%")))
  (register-keyboard-fn sdl:*scancode-volumeup* (lambda () (format #t "user pressed sdl-scancode-volumeup key ~%")))
  (register-keyboard-fn sdl:*scancode-volumedown* (lambda () (format #t "user pressed sdl-scancode-volumedown key ~%")))
  (register-keyboard-fn sdl:*scancode-lockingcapslock* (lambda () (format #t "user pressed sdl-scancode-lockingcapslock key ~%")))
  (register-keyboard-fn sdl:*scancode-lockingnumlock* (lambda () (format #t "user pressed sdl-scancode-lockingnumlock key ~%")))
  (register-keyboard-fn sdl:*scancode-lockingscrolllock* (lambda () (format #t "user pressed sdl-scancode-lockingscrolllock key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-comma* (lambda () (format #t "user pressed sdl-scancode-kp-comma key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-equalsas400* (lambda () (format #t "user pressed sdl-scancode-kp-equalsas400 key ~%")))
  (register-keyboard-fn sdl:*scancode-international1* (lambda () (format #t "user pressed sdl-scancode-international1 key ~%")))
  (register-keyboard-fn sdl:*scancode-international2* (lambda () (format #t "user pressed sdl-scancode-international2 key ~%")))
  (register-keyboard-fn sdl:*scancode-international3* (lambda () (format #t "user pressed sdl-scancode-international3 key ~%")))
  (register-keyboard-fn sdl:*scancode-international4* (lambda () (format #t "user pressed sdl-scancode-international4 key ~%")))
  (register-keyboard-fn sdl:*scancode-international5* (lambda () (format #t "user pressed sdl-scancode-international5 key ~%")))
  (register-keyboard-fn sdl:*scancode-international6* (lambda () (format #t "user pressed sdl-scancode-international6 key ~%")))
  (register-keyboard-fn sdl:*scancode-international7* (lambda () (format #t "user pressed sdl-scancode-international7 key ~%")))
  (register-keyboard-fn sdl:*scancode-international8* (lambda () (format #t "user pressed sdl-scancode-international8 key ~%")))
  (register-keyboard-fn sdl:*scancode-international9* (lambda () (format #t "user pressed sdl-scancode-international9 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang1* (lambda () (format #t "user pressed sdl-scancode-lang1 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang2* (lambda () (format #t "user pressed sdl-scancode-lang2 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang3* (lambda () (format #t "user pressed sdl-scancode-lang3 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang4* (lambda () (format #t "user pressed sdl-scancode-lang4 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang5* (lambda () (format #t "user pressed sdl-scancode-lang5 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang6* (lambda () (format #t "user pressed sdl-scancode-lang6 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang7* (lambda () (format #t "user pressed sdl-scancode-lang7 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang8* (lambda () (format #t "user pressed sdl-scancode-lang8 key ~%")))
  (register-keyboard-fn sdl:*scancode-lang9* (lambda () (format #t "user pressed sdl-scancode-lang9 key ~%")))
  (register-keyboard-fn sdl:*scancode-alterase* (lambda () (format #t "user pressed sdl-scancode-alterase key ~%")))
  (register-keyboard-fn sdl:*scancode-sysreq* (lambda () (format #t "user pressed sdl-scancode-sysreq key ~%")))
  (register-keyboard-fn sdl:*scancode-cancel* (lambda () (format #t "user pressed sdl-scancode-cancel key ~%")))
  (register-keyboard-fn sdl:*scancode-clear* (lambda () (format #t "user pressed sdl-scancode-clear key ~%")))
  (register-keyboard-fn sdl:*scancode-prior* (lambda () (format #t "user pressed sdl-scancode-prior key ~%")))
  (register-keyboard-fn sdl:*scancode-return2* (lambda () (format #t "user pressed sdl-scancode-return2 key ~%")))
  (register-keyboard-fn sdl:*scancode-separator* (lambda () (format #t "user pressed sdl-scancode-separator key ~%")))
  (register-keyboard-fn sdl:*scancode-out* (lambda () (format #t "user pressed sdl-scancode-out key ~%")))
  (register-keyboard-fn sdl:*scancode-oper* (lambda () (format #t "user pressed sdl-scancode-oper key ~%")))
  (register-keyboard-fn sdl:*scancode-clearagain* (lambda () (format #t "user pressed sdl-scancode-clearagain key ~%")))
  (register-keyboard-fn sdl:*scancode-crsel* (lambda () (format #t "user pressed sdl-scancode-crsel key ~%")))
  (register-keyboard-fn sdl:*scancode-exsel* (lambda () (format #t "user pressed sdl-scancode-exsel key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-00* (lambda () (format #t "user pressed sdl-scancode-kp-00 key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-000* (lambda () (format #t "user pressed sdl-scancode-kp-000 key ~%")))
  (register-keyboard-fn sdl:*scancode-thousandsseparator* (lambda () (format #t "user pressed sdl-scancode-thousandsseparator key ~%")))
  (register-keyboard-fn sdl:*scancode-decimalseparator* (lambda () (format #t "user pressed sdl-scancode-decimalseparator key ~%")))
  (register-keyboard-fn sdl:*scancode-currencyunit* (lambda () (format #t "user pressed sdl-scancode-currencyunit key ~%")))
  (register-keyboard-fn sdl:*scancode-currencysubunit* (lambda () (format #t "user pressed sdl-scancode-currencysubunit key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-leftparen* (lambda () (format #t "user pressed sdl-scancode-kp-leftparen key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-rightparen* (lambda () (format #t "user pressed sdl-scancode-kp-rightparen key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-leftbrace* (lambda () (format #t "user pressed sdl-scancode-kp-leftbrace key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-rightbrace* (lambda () (format #t "user pressed sdl-scancode-kp-rightbrace key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-tab* (lambda () (format #t "user pressed sdl-scancode-kp-tab key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-backspace* (lambda () (format #t "user pressed sdl-scancode-kp-backspace key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-a* (lambda () (format #t "user pressed sdl-scancode-kp-a key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-b* (lambda () (format #t "user pressed sdl-scancode-kp-b key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-c* (lambda () (format #t "user pressed sdl-scancode-kp-c key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-d* (lambda () (format #t "user pressed sdl-scancode-kp-d key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-e* (lambda () (format #t "user pressed sdl-scancode-kp-e key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-f* (lambda () (format #t "user pressed sdl-scancode-kp-f key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-xor* (lambda () (format #t "user pressed sdl-scancode-kp-xor key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-power* (lambda () (format #t "user pressed sdl-scancode-kp-power key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-percent* (lambda () (format #t "user pressed sdl-scancode-kp-percent key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-less* (lambda () (format #t "user pressed sdl-scancode-kp-less key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-greater* (lambda () (format #t "user pressed sdl-scancode-kp-greater key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-ampersand* (lambda () (format #t "user pressed sdl-scancode-kp-ampersand key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-dblampersand* (lambda () (format #t "user pressed sdl-scancode-kp-dblampersand key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-verticalbar* (lambda () (format #t "user pressed sdl-scancode-kp-verticalbar key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-dblverticalbar* (lambda () (format #t "user pressed sdl-scancode-kp-dblverticalbar key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-colon* (lambda () (format #t "user pressed sdl-scancode-kp-colon key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-hash* (lambda () (format #t "user pressed sdl-scancode-kp-hash key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-space* (lambda () (format #t "user pressed sdl-scancode-kp-space key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-at* (lambda () (format #t "user pressed sdl-scancode-kp-at key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-exclam* (lambda () (format #t "user pressed sdl-scancode-kp-exclam key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memstore* (lambda () (format #t "user pressed sdl-scancode-kp-memstore key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memrecall* (lambda () (format #t "user pressed sdl-scancode-kp-memrecall key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memclear* (lambda () (format #t "user pressed sdl-scancode-kp-memclear key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memadd* (lambda () (format #t "user pressed sdl-scancode-kp-memadd key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memsubtract* (lambda () (format #t "user pressed sdl-scancode-kp-memsubtract key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memmultiply* (lambda () (format #t "user pressed sdl-scancode-kp-memmultiply key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-memdivide* (lambda () (format #t "user pressed sdl-scancode-kp-memdivide key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-plusminus* (lambda () (format #t "user pressed sdl-scancode-kp-plusminus key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-clear* (lambda () (format #t "user pressed sdl-scancode-kp-clear key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-clearentry* (lambda () (format #t "user pressed sdl-scancode-kp-clearentry key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-binary* (lambda () (format #t "user pressed sdl-scancode-kp-binary key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-octal* (lambda () (format #t "user pressed sdl-scancode-kp-octal key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-decimal* (lambda () (format #t "user pressed sdl-scancode-kp-decimal key ~%")))
  (register-keyboard-fn sdl:*scancode-kp-hexadecimal* (lambda () (format #t "user pressed sdl-scancode-kp-hexadecimal key ~%")))
  (register-keyboard-fn sdl:*scancode-lctrl* (lambda () (format #t "user pressed sdl-scancode-lctrl key ~%")))
  (register-keyboard-fn sdl:*scancode-lshift* (lambda () (format #t "user pressed sdl-scancode-lshift key ~%")))
  (register-keyboard-fn sdl:*scancode-lalt* (lambda () (format #t "user pressed sdl-scancode-lalt key ~%")))
  (register-keyboard-fn sdl:*scancode-lgui* (lambda () (format #t "user pressed sdl-scancode-lgui key ~%")))
  (register-keyboard-fn sdl:*scancode-rctrl* (lambda () (format #t "user pressed sdl-scancode-rctrl key ~%")))
  (register-keyboard-fn sdl:*scancode-rshift* (lambda () (format #t "user pressed sdl-scancode-rshift key ~%")))
  (register-keyboard-fn sdl:*scancode-ralt* (lambda () (format #t "user pressed sdl-scancode-ralt key ~%")))
  (register-keyboard-fn sdl:*scancode-rgui* (lambda () (format #t "user pressed sdl-scancode-rgui key ~%")))
  (register-keyboard-fn sdl:*scancode-mode* (lambda () (format #t "user pressed sdl-scancode-mode key ~%")))
  (register-keyboard-fn sdl:*scancode-audionext* (lambda () (format #t "user pressed sdl-scancode-audionext key ~%")))
  (register-keyboard-fn sdl:*scancode-audioprev* (lambda () (format #t "user pressed sdl-scancode-audioprev key ~%")))
  (register-keyboard-fn sdl:*scancode-audiostop* (lambda () (format #t "user pressed sdl-scancode-audiostop key ~%")))
  (register-keyboard-fn sdl:*scancode-audioplay* (lambda () (format #t "user pressed sdl-scancode-audioplay key ~%")))
  (register-keyboard-fn sdl:*scancode-audiomute* (lambda () (format #t "user pressed sdl-scancode-audiomute key ~%")))
  (register-keyboard-fn sdl:*scancode-mediaselect* (lambda () (format #t "user pressed sdl-scancode-mediaselect key ~%")))
  (register-keyboard-fn sdl:*scancode-www* (lambda () (format #t "user pressed sdl-scancode-www key ~%")))
  (register-keyboard-fn sdl:*scancode-mail* (lambda () (format #t "user pressed sdl-scancode-mail key ~%")))
  (register-keyboard-fn sdl:*scancode-calculator* (lambda () (format #t "user pressed sdl-scancode-calculator key ~%")))
  (register-keyboard-fn sdl:*scancode-computer* (lambda () (format #t "user pressed sdl-scancode-computer key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-search* (lambda () (format #t "user pressed sdl-scancode-ac-search key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-home* (lambda () (format #t "user pressed sdl-scancode-ac-home key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-back* (lambda () (format #t "user pressed sdl-scancode-ac-back key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-forward* (lambda () (format #t "user pressed sdl-scancode-ac-forward key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-stop* (lambda () (format #t "user pressed sdl-scancode-ac-stop key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-refresh* (lambda () (format #t "user pressed sdl-scancode-ac-refresh key ~%")))
  (register-keyboard-fn sdl:*scancode-ac-bookmarks* (lambda () (format #t "user pressed sdl-scancode-ac-bookmarks key ~%")))
  (register-keyboard-fn sdl:*scancode-brightnessdown* (lambda () (format #t "user pressed sdl-scancode-brightnessdown key ~%")))
  (register-keyboard-fn sdl:*scancode-brightnessup* (lambda () (format #t "user pressed sdl-scancode-brightnessup key ~%")))
  (register-keyboard-fn sdl:*scancode-displayswitch* (lambda () (format #t "user pressed sdl-scancode-displayswitch key ~%")))
  (register-keyboard-fn sdl:*scancode-kbdillumtoggle* (lambda () (format #t "user pressed sdl-scancode-kbdillumtoggle key ~%")))
  (register-keyboard-fn sdl:*scancode-kbdillumdown* (lambda () (format #t "user pressed sdl-scancode-kbdillumdown key ~%")))
  (register-keyboard-fn sdl:*scancode-kbdillumup* (lambda () (format #t "user pressed sdl-scancode-kbdillumup key ~%")))
  (register-keyboard-fn sdl:*scancode-eject* (lambda () (format #t "user pressed sdl-scancode-eject key ~%")))
  (register-keyboard-fn sdl:*scancode-sleep* (lambda () (format #t "user pressed sdl-scancode-sleep key ~%")))
  (register-keyboard-fn sdl:*scancode-app1* (lambda () (format #t "user pressed sdl-scancode-app1 key ~%")))
  (register-keyboard-fn sdl:*scancode-app2* (lambda () (format #t "user pressed sdl-scancode-app2 key ~%")))
  (register-keyboard-fn sdl:*scancode-audiorewind* (lambda () (format #t "user pressed sdl-scancode-audiorewind key ~%")))
  (register-keyboard-fn sdl:*scancode-audiofastforward* (lambda () (format #t "user pressed sdl-scancode-audiofastforward key ~%")))
  (register-keyboard-fn sdl:*scancode-softleft* (lambda () (format #t "user pressed sdl-scancode-softleft key ~%")))
  (register-keyboard-fn sdl:*scancode-softright* (lambda () (format #t "user pressed sdl-scancode-softright key ~%")))
  (register-keyboard-fn sdl:*scancode-call* (lambda () (format #t "user pressed sdl-scancode-call key ~%")))
  (register-keyboard-fn sdl:*scancode-endcall* (lambda () (format #t "user pressed sdl-scancode-endcall key ~%")))
  (register-keyboard-fn sdl:*num-scancodes* (lambda () (format #t "user pressed sdl-num-scancodes key ~%")))
  )


;;===================


;; why everything global ? so i can change it !
(define (cleanup)
  (cairo:destroy *cr*)
  (cairo:surface-destroy *cairo-surface*) 
  (sdl:free-surface *surface*)
  (sdl:destroy-texture *texture*)
  (sdl:destroy-texture *sprites-texture*)  
  ;;(sdl:destroy-texture *sprites2-texture*)  
  (sdl:destroy-texture *sprites3-texture*)
  (sdl:destroy-texture *man-texture*)
  
  (sdl:destroy-renderer *render*)
  (sdl:destroy-window *window*)
  ;; sdl-ttf-quit
  
  (img:quit)
  (sdl:quit))



