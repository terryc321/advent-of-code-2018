
;; we can install this using
;; sudo cp -v macros/inc.scm /opt/guile/share/guile/site/3.0/macros
;;
;; so to use these macros we can simply put - and guile will find it !
;; (use-modules (macros array-loop))
;;
;; when we autoload a macro , we must put a list of symbols explicitly to import after 
;; #:autoload (ice-9 match) (match) 
;;
;; #:export (array-loop)
;; export expects a list of symbols to export macros or otherwise
;;


(define-module (macros array-loop)
  #:autoload (ice-9 match) (match)
  #:export (array-loop ))


#|

 for y from 1 to array-height do 
  for x from 1 to array-width do 
   f{x,y}

|#
(define-syntax array-loop
  (syntax-rules ()
    ((_ arr foo)
     (match (array-shape arr)
       (((x0 x1)(y0 y1))
	(let ((width x1)
	      (height y1))
	  (let loopy ((y 1))
	    (when  (<= y height)
	      (let loopx ((x 1))
		(when  (<= x width)
		  (foo arr x y)
		  (loopx (+ x 1))))
	      (loopy (+ y 1))))))))))


