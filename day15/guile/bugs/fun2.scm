;; -*- geiser-scheme-implementation: guile -*-

 ;; for a given array - find all goblins - find all elfs
(define find-elfs-goblins
  (lambda (arr)  ;; how get width height of generated array in guile ?    
    (let ((width 32)(height 32)
	  ;; (elfs '())
	  ;; (goblins '()))
	  )
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
	      ;; (let ((symbol (array-ref arr x y)))
	      ;; 	(cond
	      ;; 	 ((eq? symbol 'wall) (format #t "~a" #\#))
	      ;; 	 ((eq? symbol 'cave) (format #t "~a" #\.))
	      ;; 	 ((eq? symbol 'elf)  (set! elfs (cons (list x y) elfs))  )
	      ;; 	 ((eq? symbol 'goblin) (set! goblins (cons (list x y) goblins)) )
	      ;; 	 (#t (error "bad symbol"))))
	       ;; next x 
	      (loopx (+ x 1)))) ;; let loopx
	  ;; newline 
	  (when (<= y height)
	    ;; (format #t "~%")
	    #t
	    )
	  ;; next y	    
	  (loopy (+ y 1)))) ;; let loopy
      )))




(define find-elfs-goblins
  (lambda (arr)  
    (let ((width 5)
	  (height 7))
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
	      (format #t "x= ~a : y =~a ~%" x y)
	      (loopx (+ x 1))))
	  (when (<= y height)  #t )
	  (loopy (+ y 1)))))))




