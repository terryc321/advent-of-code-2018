
;;-*- geiser-scheme-implementation: guile -*-

(define (factorial n)
  (cond
   ((< n 2) (/ n 0))
   (#t (* n (factorial (- n 1))))))


;;(factorial 5)
