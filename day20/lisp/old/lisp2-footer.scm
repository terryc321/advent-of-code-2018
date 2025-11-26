
;; just run it - get idea of run time
(run)

;; report all open doors
(call-with-output-file "lisp2-prog-side-doors.scm"
  (lambda (port)
    (format port "(define *side-doors* '(")
    (hash-table-for-each *side-doors* (lambda (k v)
				   (format port "(~a ~a)~%" (point-x k) (point-y k))))
    (format port "))~%")))

(format #t "recorded *side-doors* in lisp2-prog-open-doors.scm~%")


(call-with-output-file "lisp2-prog-trap-doors.scm"
  (lambda (port)
    (format port "(define *trap-doors* '(")
    (hash-table-for-each *trap-doors* (lambda (k v)
				   (format port "(~a ~a)~%" (point-x k) (point-y k))))
    (format port "))~%")))

(format #t "recorded *trap-doors* in lisp2-prog-trap-doors.scm~%")



;; 
(call-with-output-file "lisp2-prog-end-points.scm"
  (lambda (port)
    (format port "(define *endpoints* '("
	    (let loop ((pts *endpoints*))
	      (cond
	       ((null? pts) #f)
	       (#t (let ((pt (car pts)))
		     (format port "(~a ~a)~%" (point-x pt) (point-y pt))
		     (loop (cdr pts)))))))
    (format port "))~%")))

(format #t "recorded *endpoints* in lisp2-prog-end-points.scm~%")

    
