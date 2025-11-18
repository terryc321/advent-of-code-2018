

(define (fac n)
  (cond
   ((< n 2) n)
   (#t (* n (fac (- n 1))))))

