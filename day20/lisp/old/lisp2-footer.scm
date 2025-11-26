
;; just run it - get idea of run time
(run)

;; report all open doors
(call-with-output-file "../output/side-doors.scm"
  (lambda (port)
    (format port "(define *side-doors* '(")
    (hash-table-for-each *side-doors* (lambda (k v)
				   (format port "(~a ~a)~%" (point-x k) (point-y k))))
    (format port "))~%")))
(format #t "recorded *side-doors* in ../output/side-doors.scm~%")


(call-with-output-file "../output/trap-doors.scm"
  (lambda (port)
    (format port "(define *trap-doors* '(")
    (hash-table-for-each *trap-doors* (lambda (k v)
				   (format port "(~a ~a)~%" (point-x k) (point-y k))))
    (format port "))~%")))
(format #t "recorded *trap-doors* in ../output/trap-doors.scm~%")


(call-with-output-file "../output/step-hash.scm"
  (lambda (port)
    (format port "(define *step-hash* '(")
    (hash-table-for-each *step-hash* (lambda (k v)
				   (format port "(~a ~a)~%" k v)))
    (format port "))~%")))
(format #t "recorded *step-hash* in ../output/step-hash.scm~%")


 
(call-with-output-file "../output/end-points.scm"
  (lambda (port)
    (format port "(define *endpoints* '(")
    (let loop ((pts *endpoints*))
      (cond
       ((null? pts) #f)
       (#t (let ((pt (car pts)))
	     (format port "(~a ~a)~%" (point-x pt) (point-y pt))
	     (loop (cdr pts))))))
    (format port "))~%")))
(format #t "recorded *endpoints* in ../output/end-points.scm~%")

    
