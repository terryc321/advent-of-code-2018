

(define original-eval-application eval-application)

(set! eval-application
  (lambda (exp env cont)
    (cond
     ((eq? (car exp) 'my)
      'my)
     (else (original-eval-application exp env cont)))))
