#|
second attempt at day 6 
fresh start ...

|#

(import scheme)
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "../day4")
;; (current-directory)


(import procedural-macros)
(import regex)

(import simple-md5)
(import simple-loops)

(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val

;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors


;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(import matchable)

;;------------------------- code -----------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

(define input (get-input))


(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))


(define (manhatten x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))


;; manhatten of square vis input
(define (man-square x y cords)
  (let ((mins '())
	(minimum #f)
	(id 0))   
    (do-list (cid cords)
	     (match cid
	       ((x2 y2)
		(set! id (1+ id))
		(let ((dist (manhatten x y x2 y2)))
		  ;;(format #t "~a,~a : ~a : ~a~%" x2 y2 id dist)
		  (cond
		   ((not minimum) (set! mins (list id))
		    (set! minimum dist))
		   ((< dist minimum) (set! mins (list id))
		    (set! minimum dist))
		   ((= dist minimum) (set! mins (cons id mins)))
		   (#t #f))))
	       (_ (error "man-square"))))
    (let ((len-mins (length mins)))
      (cond
       ((= len-mins 0) (error "man-square no minimum??"))
       ((> len-mins 1) #f)
       ((= len-mins 1) (car mins))))))



(define (vec-analysis v1 v2)
  (let ((len1 (vector-length v1))
	(len2 (vector-length v2)))
    (assert (= len1 len2))
    (let ((same '()))
      (do-for (k 0 len2)
	      (let ((a (vector-ref v1 k))
		    (b (vector-ref v2 k)))
		(cond
		 ((= a b) (set! same (cons a same))))))
      (sort same >))))



(define (brute x1 y1 x2 y2 cords prev)
  (assert (> x2 x1))
  (assert (> y2 y1))
  (let* ((dx (- x2 x1))
	 (dy (- y2 y1))
	 (area (* dx dy)))
    (format #t "computing (~a,~a) -> (~a,~a) : total area ~a ~%" x1 y1 x2 y2 area))
  (define nvec (make-vector (+ 2 (length cords)) 0))
  (do-for (x x1 x2)
	  (do-for (y y1 y2)
		  (let ((m (man-square x y cords)))
		    (cond
		     ((integer? m) (vector-set! nvec m (1+ (vector-ref nvec m))))))))
  (format #t "~a~%" nvec)
  (cond
   (prev (format #t "analysis ~%")
	 (format #t "~a ~%" (vec-analysis nvec prev))))
  (brute (10- x1) (10- y1) (10+ x2) (10+ y2) cords nvec))



(define (part-1)
  (let* ((lo-x (apply min (map first input)))
	 (hi-x (apply max (map first input)))
	 (lo-y (apply min (map second input)))
	 (hi-y (apply max (map second input)))
	 (prev #f))
    (brute lo-x lo-y hi-x hi-y input prev)))



(part-1)

#|

#(0 1909 3188 11748 3538 196215 24222 32370 17333 1763 1124 12223 747 16507 1155 1699 31966 18132 707 12719 1865 1003 3687 19457 1662 2595 228385 1287 3402 207273 1402 2589 41072 3036 23751 39254 26181 1202 1779 1114 12612 12595 1902 12888 8776 7004 22181 2536 999 2436 199518 0)
#(0 1909 3188 12008 3538 205175 24722 33110 17713 1763 1124 12483 747 16877 1155 1699 32696 18542 707 12979 1865 1003 3687 19887 1662 2595 238075 1287 3402 216483 1402 2589 42042 3036 24301 40084 26771 1202 1779 1114 12892 12865 1902 13118 8936 7124 22701 2536 999 2436 208558 0)

|#
#|
(vec-analysis 
 #(0 1909 3188 11748 3538 196215 24222 32370 17333 1763 1124 12223 747 16507 1155 1699 31966 18132 707 12719 1865 1003 3687 19457 1662 2595 228385 1287 3402 207273 1402 2589 41072 3036 23751 39254 26181 1202 1779 1114 12612 12595 1902 12888 8776 7004 22181 2536 999 2436 199518 0)
 #(0 1909 3188 12008 3538 205175 24722 33110 17713 1763 1124 12483 747 16877 1155 1699 32696 18542 707 12979 1865 1003 3687 19887 1662 2595 238075 1287 3402 216483 1402 2589 42042 3036 24301 40084 26771 1202 1779 1114 12892 12865 1902 13118 8936 7124 22701 2536 999 2436 208558 0))
;; results analysis
 (3687 3538 3402 3188 3036 2595 2589 2536 2436 1909 1902 1865 1779 1763 1699 1662 1402 1287 1202 1155 1124 1114 1003 999 747 707 0 0)
3687 is same ...
|#

