
(defpackage :fun
	    (:use :cl))

(in-package :fun)

;; (defun get-input()
;;   (with-
;;   (call-with-input-file "../input.txt"
;;     (lambda (port)
;;       (let loop ((chars '()))
;; 	(let ((ch (read-char port)))
;; 	  (cond
;; 	   ((eof-object? ch) (reverse chars))
;; 	   ((char=? ch #\newline) (loop chars))
;; 	   ((char=? ch #\^) (loop chars))
;; 	   ((char=? ch #\$) (loop chars))	   
;; 	   (t (loop (cons ch chars)))))))))


;; parser for grammar
;; N E S W
;; ( )
;; |

(defparameter  input nil) ;;(list->vector (get-input)))
;; input is 14198 chars long - exclusing ^ $ and newlines
;; is there the functional equivalent of this
;; cdr would advance
;; 

;; i  index into string
;; pv parse vector
;; pvlen parse vector length
;; pstack parse stack 
(defparameter i 0)
(defparameter s nil)
(defparameter slen 0)
(defparameter stack nil)

(defun advance ()
  (setq i (+ i 1)))

(defun push-stack (s)
  (setq stack (cons s stack)))

(defun pop-stack()
  (let ((top (car stack)))
    (setq stack (cdr stack))
    top))

(defun cur-is(c)
  (cond
    ((eof-p) nil)
    (t (char= c (char s i)))))

(defun cur()
  (cond
    ((eof-p) nil)
    (t (char s i))))

(defun eof-p()
  (cond
    ((< i slen) nil)
    (t t)))


(defun open-p() (cur-is #\( ))
(defun close-p() (cur-is #\) ))
(defun alt-p () (cur-is #\| ))
(defun dir-p () (or (cur-is #\N) (cur-is #\S) (cur-is #\E) (cur-is #\W)))


;; ==========  N S E W  ================================
;; sequence of directions NSEWS get pushed onto stack as a list 
(defun parse-direction()
  (cond
    ((dir-p) (push-stack (cur))
     (advance))
    (t nil)))

(defun parse-word()
  (format t "parse-word => ~a~%" (cur))
  (cond
    ((dir-p)
     (let ((chars '()))
       (loop while (dir-p) do
	 (setq chars (cons (char s i) chars))
	 (advance))
       (push-stack (reverse chars))))))




;; ==========  N S E W  ================================
;; a | b | c | d ... 
(defun parse-alt()
  (parse-word)
  (cond
    ((alt-p)
     (advance)
     (let ((a (car stack)))
       (setq stack (cdr stack))
       (format t "ALT !")
       (parse-parens)
       (cond
	 ((null stack) (push-stack a))
	 (t (let ((b (car stack)))
	      (setq stack (cdr stack))
	      (push-stack (list 'alt a b)))))))))



(defun parse-parens()
  (cond
   ((open-p)
    (advance)
    (let ((dirs nil))
      (loop while (not (or (alt-p)
			   (close-p)
			   (eof-p)))
	    do
	       (parse-parens)
	       (setq dirs (cons (pop-stack) dirs)))
      (cond
	((close-p)  (advance)
	 (push-stack (cons 'seq (reverse dirs))))
	(t (format t "parens: line132: parse error - no closing parens found at index ~a~%" i)
	   (loop for k from 0 to (- i 1) do
	     (format t "~a" (char s k))
		 )
	   (format t "=>~a" (char s i))
	   (format t "<=**")
	   (loop for k from (+ i 1) to slen do
	     (format t "~a" (char s k)))))))
   (t ;; pass-through
    (parse-alt))))



;; ===== implicit seqeunce ===========
(defun parse-seq ()
  (cond
    ((eof-p) nil)
    (t (let ((dirs '()))
	 (loop while (not (or (alt-p)
			      (close-p)
			      (eof-p)))
	    do
	       (parse-parens)
	       (setq dirs (cons (pop-stack!) dirs)))
	 (push-stack (reverse dirs))))))



(defun halt-and-burn ()
  (format *standard-output* "crash : i = ~a " i)
  (format *standard-output* "crash : stack = ~a " stack))
  
(defun parse(item)
  (setq i 0)
  (setq s item)
  (setq slen (length s))
  (setq stack '())
  (parse-word)
  (pop-stack))

  

