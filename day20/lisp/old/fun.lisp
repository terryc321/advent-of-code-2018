
(defpackage :fun
	    (:use :cl))

(in-package :fun)

(declaim (optimize (debug 3) (space 1) (speed 1)(compilation-speed 1)))

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

;; input.txt - almost original
;; cropped version drop initial ^hat and $dollar ending and newlines
;; adjusted.txt - translated |) => |?) using text editor
;;

;; removed ^ and $
;; trim end newline - due uiop
;; wrapped entire thing in a parens ( input )
;; so everything will parse as a sequence
(defparameter *input*
  (concatenate 'string
	       "("
	       (let* ((str  (with-open-file (stream "adjusted.txt")
			      (uiop:read-file-string stream)))
		      (len (length str)))
		 (subseq str 0 (- len 2)))
	       "))"))


  

;; replaces |) with |?) so we have a token to get hold of 



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
    ((<= i slen) nil)
    (t t)))


(defun open-p() (cur-is #\( ))
(defun close-p() (cur-is #\) ))
(defun alt-p () (cur-is #\| ))
(defun dir-p ()
  (or (cur-is #\N) (cur-is #\S) (cur-is #\E) (cur-is #\W) (cur-is #\?)))


;; ==========  N S E W  ================================
;; sequence of directions NSEWS get pushed onto stack as a list 
;; (defun parse-direction()
;;   (cond
;;     ((dir-p) (push-stack (cur))
;;      (advance))
;;     (t nil)))

(defun parse-word()
  ;; (format t "parse-word => ~a~%" (cur))
  (cond
    ((dir-p)
     (let ((chars '()))
       (loop while (and (not (eof-p)) (dir-p)) do
	 (setq chars (cons (char s i) chars))
	 (advance))
       (push-stack (reverse chars))))
    (t (format t "are you sure => ~a is N S E W ?" (cur)))))





;; ==========  N S E W  ================================
;; a | b | c | d ... 
(defun parse-alt()  
  (cond
    ((dir-p) (parse-word))
    ((alt-p)
     (let ((a (car stack)))
       (setq stack (cdr stack))
       (let ((alts (list a)))
	 (loop while (and (not (eof-p)) (alt-p)) do
	   (advance)
	   (parse-expr)
	   ;; expecting stack to have something on it
	   (setq alts (cons (pop-stack) alts)))
	 (push-stack (cons 'alt (reverse alts))))))))


;; should slurp everything until meet matching closing parens
;; nothing above should accept a closing or open parens - thats this routines job
(defun parse-parens()
  (cond
   ((open-p)
    (advance)
    (let ((dirs nil))
      (loop while (not (or 
			   (close-p)
			   (eof-p)))
	    do
	       (parse-expr)
	       ;; expecting stack to have something on it
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



;; ;; ===== implicit seqeunce ===========
;; (defun parse-seq ()
;;   (cond
;;     ((eof-p) nil)
;;     (t (let ((dirs '()))
;; 	 (loop while (not (or (alt-p)
;; 			      (close-p)
;; 			      (eof-p)))
;; 	    do
;; 	       (parse-parens)
;; 	       (setq dirs (cons (pop-stack!) dirs)))
;; 	 (push-stack (reverse dirs))))))


(defun parse-expr ()
  (cond
    ((eof-p) nil)
    (t (parse-parens))))





(defun halt-and-burn ()
  (format *standard-output* "crash : i = ~a " i)
  (format *standard-output* "crash : stack = ~a " stack))
  
(defun parse(item k)
  (setq i k)
  (setq s item)
  (setq slen (- (length s) 1))
  (setq stack '())
  (parse-expr)
  (pop-stack))


;; parse each section and then substring the rest
;; each parsed item concatenated together should result in the original text

(defun run (item)
  (let ((k 0)
	(trees nil))
    (setq i k)
    (setq s item)
    (setq slen (- (length s) 1))
    (loop while (<= i slen) do
      (setq stack '())
      (parse-expr)
      (let ((out (pop-stack)))      
	(setq trees (cons out trees))))
    trees))





(defun run2 (item)
  (let ((k 0)
	(trees nil))
    (setq i k)
    (setq s item)
    (setq slen (- (length s) 1))
    (eof-p)))

;; correct input is mis-parens 
(defun binding (s)
  (let ((left 0)(right 0))
    (loop for i from 0 to (- (length s) 1) do
      (cond
	((char= (char s i) #\( ) (incf left))
	((char= (char s i) #\) ) (incf right))))
    (list left right)))






