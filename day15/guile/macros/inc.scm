
;; we can install this using
;; sudo cp -v macros/inc.scm /opt/guile/share/guile/site/3.0/macros
;;
;; so to use these macros we can simply put - and guile will find it !
;; (use-modules (macros inc))
;;
;;
;; inc.scm
;; inc!.scm does funny things in bash so we just call it inc.scm

(define-module (macros inc)
  #:export (inc!
	    dec!
	    ))

(define-syntax inc!
  (syntax-rules ()
    ((_ x) (set! x (+ x 1)))
    ((_ x n) (set! x (+ x n)))))

;; (let ((x 1))(inc! x) x) => 2
;; (let ((x 1))(inc! x 2) x) => 3

(define-syntax dec!
  (syntax-rules ()
    ((_ x) (set! x (- x 1)))
    ((_ x n) (set! x (- x n)))))

;; (let ((x 1))(dec! x) x) => 0
;; (let ((x 1))(dec! x 2) x) => -1
