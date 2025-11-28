(defpackage :pretty (:use :cl))

(in-package :pretty)

#|
unfortunate situation cannot pretty print lisp code
common lisp
read in form and simply pretty it out
|#

(defun slurp (filename)
  (with-open-file (stream "fun.lisp" :direction :input)
    (let* ((exprs '()))	   
      (catch 'foo
	(loop while t do
	  (let ((expr (read stream nil nil)))
	    (cond
	      ((eq expr nil) (throw 'foo (reverse exprs)))
	      (t (setq exprs (cons expr exprs))))))))))

(defun pretty (exprs)
  (loop for x in exprs do
    (write "~a~%" (string-downcase (write x nil)format nil "~a~%" x)))))

;; (write "ASDF" :stream nil :case :DOWNCASE)

