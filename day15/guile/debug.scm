;; -*- geiser-scheme-implementation: guile -*-

(use-modules (ice-9 match))
(use-modules (ice-9 format))


#|

 for y from 1 to array-height do 
  for x from 1 to array-width do 
   f{x,y}

|#
(define-syntax array-loop
  (syntax-rules ()
    ((_ arr foo)
     (match (array-shape arr)
       (((x0 x1)(y0 y1))
	(let ((width x1)
	      (height y1))
	  (let loopy ((y 1))
	    (when  (<= y height)
	      (let loopx ((x 1))
		(when  (<= x width)
		  (foo arr x y)
		  (loopx (+ x 1))))
	      (loopy (+ y 1))))))))))
	

;; (define find-elfs-goblins
;;   (lambda (arr foo) 
;;     (let ((width 32)(height 32)
;; 	  (elfs '())
;; 	  (goblins '()))
;;       (let loopy ((y 1))
;; 	(when  (<= y height)
;; 	  (let loopx ((x 1))
;; 	    (when  (<= x width)
;;               (foo arr x y)
;; 	      (loopx (+ x 1))))
;; 	  (loopy (+ y 1)))))))

;; (define elfs '())
;; (define goblins '())

(define (find-elfs-goblins arr)
  (let ((elfs '())
	(goblins '()))
    (let ((foo (lambda (arr x y)
		 (let ((symbol (array-ref arr x y)))
		   (cond
		    ((eq? symbol 'wall) #f)
		    ((eq? symbol 'cave) #f)
		    ((eq? symbol 'elf)  (set! elfs (cons (list x y) elfs))  )
		    ((eq? symbol 'goblin) (set! goblins (cons (list x y) goblins)))
		    (#t (error "bad symbol")))))))
      (array-loop arr foo)
      (values elfs goblins))))

;; (let-values ((elfs goblins) (find-elfs-goblins arr))
;;   elfs)


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



(define (process)
  (set! elfs '())
  (set! goblins '())
  (array-loop arr find-elfs-goblins2)
  (format #t "elfs = ~a~%" elfs)
  (format #t "goblins = ~a~%" goblins))

  
(define dummy
  (lambda (arr x y)
    (format #t "looking at {~a} at {~a,~a}~%" arr x y)))



(define make-simple-fn
  (lambda (arr x y)
    (let ((symbol (array-ref arr x y)))
      (cond
        ((eq? symbol 'wall) (format #t "~a" #\#))
        ((eq? symbol 'cave) (format #t "~a" #\.))
        ((eq? symbol 'elf)  (set! elfs (cons (list x y) elfs))  )
        ((eq? symbol 'goblin) (set! goblins (cons (list x y) goblins)) )
        (#t (error "bad symbol"))))))

	      



