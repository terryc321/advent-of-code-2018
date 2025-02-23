;;;; fun.lisp

(in-package #:aoc18)

;;(ppcre:all-matches-as-strings "\\d+" "numbers: 1 10 42")
(defun read-from-file (filename)
  (labels ((parse-if-string (x)
	     (cond
	       ((stringp x) (parse-integer x))
	       (t x))))
  (let ((xs nil)
	(in (open filename :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do 
            (format t "~a~%" line)
	    (let* ((has-digits (ppcre:all-matches-as-strings "\\d+" line))
		   (int-values (or
				(ppcre:register-groups-bind (x1 y1 y2)
				    ("x=(\\d+).*y=(\\d+)\.\.(\\d+)" line :sharedp t)
				  (mapcar #'parse-if-string (list 'x-y-y x1 y1 y2)))
				(ppcre:register-groups-bind (x1 y1 y2)
				    ("y=(\\d+).*x=(\\d+)\.\.(\\d+)" line :sharedp t)
				  (mapcar #'parse-if-string (list 'y-x-x x1 y1 y2))))))
	      (cond
		((and has-digits (not int-values)) (error (format nil "bad read on ~a" line)))
		(int-values 
		 (setq xs (cons int-values xs)))))))
    (setq xs (reverse xs))
    xs)))


;; read lines from
(defparameter input (read-from-file "../input.txt"))
(defparameter example (read-from-file "../example.txt"))





