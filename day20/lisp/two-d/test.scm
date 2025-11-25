

;; (define iv (make-interval '#(1 1) '#(11 11)))
;; (define a (make-array iv (lambda () 0)))


(define (make-2d-array rows cols init)
  (let ((v (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((= i rows) v)
      (vector-set! v i (make-vector cols init)))))

(define (array-ref2 a i j)
  (vector-ref (vector-ref a i) j))

(define (array-set2! a i j val)
  (vector-set! (vector-ref a i) j val))

