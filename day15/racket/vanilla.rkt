;; -*- geiser-scheme-implementation: racket -*-

#|
a functional 2d array ?

|#

(define make-elf (lambda (h) (let ((hits h))
                              (lambda (t)
                                (cond
                                  ((eq? t 'type) 'elf)
                                  ((eq? t 'hits) hits))))))
(define make-goblin (lambda (h) (let ((hits h))
                                 (lambda (t)
                                   (cond
                                     ((eq? t 'type) 'goblin)
                                     ((eq? t 'hits) hits))))))
(define make-wall (lambda () 'wall))
(define make-cave (lambda () 'cave))

(define elf? (lambda (f) (eq? (f 'type) 'elf)))
(define goblin? (lambda (f) (eq? (f 'type) 'goblin)))


(define (test)
  (let ((g (make-goblin 5)))
    (format #t "goblin? => ~a~%" (goblin? g))
    (format #t "elf? => ~a~%" (elf? g)))
  
  (let ((e (make-elf 5)))
    (format #t "goblin? => ~a~%" (goblin? g))
    (format #t "elf? => ~a~%" (elf? g))))




