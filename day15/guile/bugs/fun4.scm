


(define find-elfs-goblins
  (lambda (arr)  
    (let ((width 2)
	  (height 3))
      (let loopy ((y 1))
	(when  (<= y height)
	  (let loopx ((x 1))
	    (when  (<= x width)
	      (print "x is ")
	      (print x)
	      (print " y is ")
	      (print y)
	      (newline)
	      (loopx (+ x 1))))
	  (when (<= y height)  #t )
	  (loopy (+ y 1)))))))






