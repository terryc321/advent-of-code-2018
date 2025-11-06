;; -*- geiser-scheme-implementation: guile -*-


 ;; for a given array - find all goblins - find all elfs
(define find-elfs-goblins
  (lambda (arr foo)  ;; how get width height of generated array in guile ?    
    (let ((width 32)(height 32)
	  (elfs '())
	  (goblins '()))
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
              (foo arr x y)
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
      


(define simple-fn
  (lambda (arr x y)
    (let ((symbol (array-ref arr x y)))
      (cond
        ((eq? symbol 'wall) (format #t "~a" #\#))
        ((eq? symbol 'cave) (format #t "~a" #\.))
        ((eq? symbol 'elf)  (set! elfs (cons (list x y) elfs))  )
        ((eq? symbol 'goblin) (set! goblins (cons (list x y) goblins)) )
        (#t (error "bad symbol"))))))

	      



