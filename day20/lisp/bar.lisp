
(defpackage :fun
	    (:use :cl))

(in-package :fun)

;; put a ( ... ) around input form an implicit sequence 
;;(parse "ASDF")
;; pre-tokenised - each character is a token
;; split input into several chunks based on level of alternative
;; ?|?|? => 3 chunks
;; if split chunks + alt symbol together => should get 
;;
;; 1 + 2 * 3 - 4 * 6
;;
;; ( ) used for grouping
;; | only operator
;; 
;; (a|b|)  <- a / b / empty-slot!
;;

;; cropped version drop initial ^hat and $dollar ending and newlines
(defparameter *input* 
  (with-open-file (stream "cropped.txt")
    (uiop:read-file-string stream)))

;; replaces |) with |?)
;; so we know ? is an empty string placeholder 

(defun viz(s)
  (let* ((len (length s))
	 (end (- len 1))
	 (level 0)
	 (maxlevel 0)
	 (hash (make-hash-table)))
    (loop for i from 0 to end do
      (cond
	((char= (char s i) #\| ) (setf (gethash i hash) (list level #\*)))
	((char= (char s i) #\N ) (setf (gethash i hash) (list level #\N)))
	((char= (char s i) #\S ) (setf (gethash i hash) (list level #\S)))
	((char= (char s i) #\E ) (setf (gethash i hash) (list level #\E)))
	((char= (char s i) #\W ) (setf (gethash i hash) (list level #\W)))
	((char= (char s i) #\( ) (setq level (+ level 1)))
	((char= (char s i) #\) ) (setq level (- level 1))))
      (when (> level maxlevel)
	(setq maxlevel level)))
    (format t "max level of string was ~a~%" maxlevel)
    (loop for v from 0 to maxlevel do
      (format t "~%~a :: " v)
      (loop for i from 0 to end do
	(let ((entry (gethash i hash nil)))
	  (cond
	    ((null entry) (format t " "))
	    (t (cond
		 ((= v (first entry)) (format t "~a" (second entry)))
		 (t (format t " "))))))))
    hash))



;; (viz "((NESW)|(NESW))|(N|E|SW|)")
;; max level of string was 2
;;
;; 0 ::                *         
;; 1 ::        *         N*E*SW* 
;; 2 ::   NESW   NESW            

	
;; repeatedly scan input following rules
;; continuous set of N S E or W become one unit - order preserved
;; N E S W E -> *1* represents N E S W E
;;
;; always check consistency as should be able to reproduce original string on all iterations
;; just end up with smaller and smaller parts as they get replaced with a pointer


(defun expand (s)
  (let ((xs '()))
    (loop for i from 0 to (- (length s) 1) do
      (let ((ch (char s i)))
	(cond
	  ((member ch '(#\N #\S #\E #\W))
	   (setq xs (cons (list 'char ch) xs)))
	  ((member ch '(#\| ))
	   (setq xs (cons (list 'alt-sym) xs)))
	  ((member ch '(#\( ))
	   (setq xs (cons (list 'open-sym) xs)))
	  ((member ch '(#\) ))
	   (setq xs (cons (list 'close-sym) xs)))
	  (t nil))))
    (reverse xs)))


(defun compress-chars (xs)
  (cond
    ((null xs) nil)
    ((eq (car (car xs)) 'char) (compress-chars-active xs (list (car xs))))
    (t (cons (car xs) (compress-chars (cdr xs))))))

(defun compress-chars-active (xs rv)
  (cond
    ((null xs) (list (list 'compress-chars (reverse rv))))
    ((eq (car (car xs)) 'char) (compress-chars-active xs (cons (car xs) rv)))
    (t (cons (list 'compress-chars (compress-chars (cons (car xs) (compress-chars (cdr xs))))))
  










  


	


