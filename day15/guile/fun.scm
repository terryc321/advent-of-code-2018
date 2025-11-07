;; -*- geiser-scheme-implementation: guile -*-

#|

enjoy experience trying to load my own sdl ffi library into guile scheme - from game1 

load + compile game1.scm first , since everything is practically at toplevel and open 

|#


(use-modules (ice-9 optargs)) ;; optional args
(use-modules (system foreign)) ;; %null-pointer
(use-modules (system foreign-library))
(use-modules (rnrs bytevectors))
(use-modules (ice-9 rdelim))
;; rdelim read-line 

(use-modules (ice-9 match))
(use-modules (ice-9 format))


(use-modules (ice-9 pretty-print)) 
(define pp pretty-print)

;; these modules are not in guile ecosystem they require an altered %load-path to make them visible
(use-modules (macros fcase))
(use-modules (macros inc))
(use-modules (macros array-loop))
(use-modules ((graphics sdl2 sdl) #:prefix sdl:))
(use-modules ((graphics sdl2 image) #:prefix img:))
(use-modules ((graphics cairo cairo) #:prefix cairo:))

;; current directory
;; (getcwd)
;; change directory
;; (chdir "day15/guile")

;; assertions ?
(define assert
  (lambda (x)
    (if x #t (error "assertion failure <where? >"))))

;;(assert #f)

;; ;; no srfi-25 !!!! WHY !!!! instead guile has inbuilt procedures 
;; ;; should be a 2d array indexes starting from 1..2 , 1..2 
;; (define arr #2@1@1((1 2) (3 4)))
;; (array-ref arr 1 1) ;; expect 1
;; (array-ref arr 2 1) ;; expect 3
;; (array-ref arr 1 2) ;; expect 2
;; (array-ref arr 2 2) ;; expect 4

;; (define arr2 (make-array #\# '(1 3) '(1 3)))
;; ;; #2@1@1((#\# #\# #\#) (#\# #\# #\#) (#\# #\# #\#))
;; ;; meaning 2d array , indexes start at 1 in x direction , 1 in y direction
;; (array-set! arr2 'fred 1 1 )
;; ;; arr2

;; 
;; all correct 
;; so we can read a file a file , now lets translate this into a 2d array where we get get access to each element

;; 
(define (get-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let ((lines '()))
	(let loop ()
	  (let ((got (read-line (current-input-port))))
	    (cond
	     ((eof-object? got) #f)
	     (#t ;;(format #t "~a~%" got)
	      (set! lines (cons got lines))
	      (loop)))))
	(reverse lines)
	))))

(define (input)
  (let* ((str-lines (get-lines "../input.txt"))
	 (nlines (length str-lines))
	 (nwidth (string-length (car str-lines)))
	 (all-nwidth (filter (lambda (n) (not (= n nwidth))) (map string-length str-lines))))
    ;; assertions survive compilation
    (assert (null? all-nwidth))
    ;; (format #t "all lines have width ~a~%" nwidth)
    ;; make an oversize array - using 1 indexing to N indexing !! 
    (let* ((w nwidth)
	   (h nlines)
	   (arr (make-array #\# (list 1 w) (list 1 h)))
	   (j 0))
      (map (lambda (str)
	     (let ((i 0))
	       (map (lambda (ch)
		      ;;(format #t "char at ~a ~a is ~a ~%" i j ch)
		      ;; must be elf E goblin G wall # or cavern . 
		      (assert (or (char=? ch #\#) (char=? ch #\.) (char=? ch #\E) (char=? ch #\G)))
		      (let ((cnv 
			     (cond 
			       ((char=? ch #\#) 'wall)
			       ((char=? ch #\.) 'cave)
			       ((char=? ch #\E) 'elf)
			       ((char=? ch #\G) 'goblin)
			       (#t (error (format #f "bad char {~a} at {line ~a,col ~a}" ch j i))))))
			(array-set! arr cnv (+ i 1) (+ j 1)))
		      (set! i (+ i 1)))
		    (string->list (list-ref str-lines j))))
	     (set! j (+ j 1)))
	   str-lines)
      (values arr w h))))

(define arr (input))


(define (output arr)
  (with-output-to-file "output.txt"
    (lambda ()  ;; how get width height of generated array in guile ?
      (let ((width 32)(height 32))
	(let loopy ((y 1))
	  (when  (<= y height)
	    (let loopx ((x 1))
	      (when  (<= x width)
		(let ((symbol (array-ref arr x y)))
		  (cond
		   ((eq? symbol 'wall) (format #t "~a" #\#))
		   ((eq? symbol 'cave) (format #t "~a" #\.))
		   ((eq? symbol 'elf) (format #t "~a" #\E))
		   ((eq? symbol 'goblin) (format #t "~a" #\G))
		   (#t (error "bad symbol"))))
		;; next x 
		(loopx (+ x 1)))) ;; let loopx
	    ;; newline 
	    (when (<= y height)
	      (format #t "~%"))
	    ;; next y	    
	    (loopy (+ y 1)))) ;; let loopy
	))))

;; we can check we can output exactly same file as input.txt
;; (output (input))
;; diff ../input.txt output.txt
;; correctly rebuilds the maze

(define show-array 
  (lambda (arr)  ;; how get width height of generated array in guile ?
    (let ((width 32)(height 32))
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
	      (let ((symbol (array-ref arr x y)))
		(cond
		 ((eq? symbol 'wall) (format #t "~a" #\#))
		 ((eq? symbol 'cave) (format #t "~a" #\.))
		 ((eq? symbol 'elf) (format #t "~a" #\E))
		 ((eq? symbol 'goblin) (format #t "~a" #\G))
		 (#t (error "bad symbol"))))
	      ;; next x 
	      (loopx (+ x 1)))) ;; let loopx
	  ;; newline 
	  (when (<= y height)
	    (format #t "~%"))
	  ;; next y	    
	  (loopy (+ y 1)))) ;; let loopy
      )))




 ;; for a given array - find all goblins - find all elfs
(define find-elfs-goblins
  (lambda (arr)  ;; how get width height of generated array in guile ?    
    (let ((width 32)(height 32)
	  (elfs '())
	  (goblins '()))
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
	      (let ((symbol (array-ref arr x y)))
		(cond
		 ((eq? symbol 'wall) #f)
		 ((eq? symbol 'cave) #f)
		 ((eq? symbol 'elf)  (set! elfs (cons (list x y) elfs))  )
		 ((eq? symbol 'goblin) (set! goblins (cons (list x y) goblins)) )
		 (#t (error "bad symbol"))))
	      ;; next x 
	      (loopx (+ x 1)))) ;; let loopx
	  ;; newline 
	  ;; (when (<= y height)
	  ;;   ;; (format #t "~%")
	  ;;   #t
	  ;;   )
	  ;; next y	    
	  (loopy (+ y 1)))) ;; let loopy
      (values elfs goblins))))


(define (goblins arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      (map (lambda (p)
	     (match p
	       ((x y) (format #t "at {~a,~a} is {~a}~%" x y (array-ref arr x y)))))
	   gobs)
      gobs)))

(define (elfs arr)
  (call-with-values (lambda () (find-elfs-goblins arr))
    (lambda (efs gobs)
      (map (lambda (p)
	     (match p
	       ((x y) (format #t "at {~a,~a} is {~a}~%" x y (array-ref arr x y)))))
	   efs)
      efs)))


#|

determine shortest path between an elf and a goblin
determine shortest paths between an? goblin and elfs
any elf in reach of a goblin < > ^ v not diagonally , then attacks rather than moves
does the shortest path move around an already present E elf or G goblin ?

need to make elfs and goblins records or objects as they also now have properties
such as health 


|#


