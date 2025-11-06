

(use-modules (ice-9 format))

(define find-elfs-goblins
  (lambda (arr)  
    (let ((width 2)
	  (height 3))
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
	      (format #t "x= ~a : y =~a ~%" x y)
	      (loopx (+ x 1))))
	  (when (<= y height)  #t )
	  (loopy (+ y 1)))))))






