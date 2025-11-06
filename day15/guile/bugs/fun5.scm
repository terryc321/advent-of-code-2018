


(define find-elfs-goblins
  (lambda (arr)  
      (let loopy ((y 1))
	(when  (<= y 2)
	  (let loopx ((x 1))
	    (when  (<= x 3)
	      (print "x is ")
	      (print x)
	      (print " y is ")
	      (print y)
	      (newline)
	      (loopx (+ x 1))))
	  (when (<= y 2)  #t )
	  (loopy (+ y 1))))))







