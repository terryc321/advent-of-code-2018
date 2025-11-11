
(use-modules (ice-9 format))
(use-modules (ice-9 pretty-print))

(define pp pretty-print)

(define head car)
(define (tail x) (force (cdr x)))
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

;; (use-modules (srfi srfi-41))
;; (define-stream (ints-f n)
;;   (stream-cons n (ints-f (1+ n))))

(define scheme-apply apply)
(load "env.scm")
;;(load "black.scm") or (load "black-with-delta.scm")
;;(load "black.scm")
(load "red.scm")

;;(format #t "the black system is ready to start => (black)")
(format #t "the red system is ready to start => (red)")


;; cannot continue to include code here as (red) does not return
