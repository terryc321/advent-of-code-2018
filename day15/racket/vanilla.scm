;; -*- geiser-scheme-implementation: guile -*-

(use-modules (ice-9 format))
(use-modules (srfi srfi-1))


;; using a closure as a datatype 
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


(define make-grid (lambda (w h)
		    (cond
		     ((not (integer? w)) (format #t "make-grid <w> h : w should be an integer~%"))
		     ((not (integer? h)) (format #t "make-grid w *h* : h should be an integer~%"))
		     ((< w 1) (format #t "make-grid <w> h : w should be positive~%"))
		     ((< h 1) (format #t "make-grid w <h> : h should be positive~%"))
		     (#t
		      (let ((contents '()))
			(lambda (t . args)
                          (cond
			   ((eq? t 'width) w)  
			   ((eq? t 'height) h)
			   ((eq? t 'insert)
			    (cond
			     ((null? args) (format #t "insert ignored- requires arg~%"))
                             ((eq? t 'remove) hits))))))))))



(define elf? (lambda (f) (eq? (f 'type) 'elf)))
(define goblin? (lambda (f) (eq? (f 'type) 'goblin)))



(define (test)
  (let ((g (make-goblin 5)))
    (format #t "goblin? => ~a~%" (goblin? g))
    (format #t "elf? => ~a~%" (elf? g)))
  
  (let ((e (make-elf 5)))
    (format #t "goblin? => ~a~%" (goblin? e))
    (format #t "elf? => ~a~%" (elf? e))))


;; make a N by N vector
;; copy that structure
;; create A , B = copy A , change B does A change also - independence





