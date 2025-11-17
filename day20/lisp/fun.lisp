


(+ 1 2)

(defun fact (n)
  (cond
    ((< n 1) (/ n 0))
    (t (*  n (fact (- m 1))))))

(fact 5)
