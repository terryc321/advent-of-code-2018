
;; lets try convert to smalltalk - maybe a change in perspective will
;; yield a solution

;; SEQ
;; ALT
;; symbols NSEW
;; symbol ?

;; convert each node into an ID
;; then we can write something like this 
;; (defparameter *t4132* (seq *t1* *t2*))
;;
;;



;; lack of control over record type unfortunate
;; -- need the open doors +plus+ the final end points 
;; (open-door! x y)
;; (make-union)
;; (union-points union)
;; (union-add union pts)

;; (define (make-union)
;;   (make-union (make-hash-table)))
;; -


#|

(define foo (make-union (make-hash-table)))
(set! union (union-add foo (list (make-point 1 2)(make-point 3 4)(make-point 5 6))))
union
(set! union (union-add foo (list (make-point 1 2)(make-point 3 4)(make-point 5 6))))
union
(union-points union)

|#


;; ;; current no open doors 
;; (define *side-doors* (make-hash-table))
;; (define *trap-doors* (make-hash-table))

;; remember what node id we are on 
(define id
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (when (> count *max-count*)
	(set! *max-count* count))
      count)))

;;============================================================================================

			       

;; ===================================================================


(define (trav-seq t p)
  (let loop ((es (cdr t)) (pts p))
    (cond
     ((null? es) pts)
     (#t (let ((e (car es)))
	   (let ((pts2 (trav e pts)))	     
	     (loop (cdr es) pts2)))))))


(define (trav-alt t pts)
  (let ((un (make-union (make-hash-table))))
    (let loop ((es (cdr t)))
      (cond
       ((null? es) (union-points un))
       (#t (let ((e (car es)))
	     (let ((out (trav e pts)))
	       (set! un (union-add un out))
	       (loop (cdr es)))))))))


;;============================================================================================


(define (trav2 t)
  (cond
   ((node-str? t) (trav2-str t))
   ((node-seq? t) (trav2-seq t))
   ((node-alt? t) (trav2-alt t))
   (#t (error "trav2 unknown"))))

;; we have a string N S E W
;; produce the path taken given a point
(define (trav2-str t)
  (let* ((s (format #f "~a" (node-data t)))
	 (id (node-id t))
	 (slen (string-length s))
	 (path (list (list 0 0)))
	 (x 0)
	 (y 0)
	 (sym (string->symbol (format #f "t~a" id))))

    ;; ensure s is a string
    (format *stdout* "~%(loading)~%")
    (format *stdout* "~%(set! ~a (lambda (pts) ~%" sym)
    (format *stdout* "(report)~%")    
    (format *stdout* " (map (lambda (pt) ~%" sym)
    (format *stdout* " (let ((x (point-x pt))(y (point-y pt))) ~%" sym)    
    (cond
     ((string=? s "?")  (format *stdout* ";; ? : nop ~%") #f)
     (#t 
      (let loop ((i 0))
	(when (< i slen) ;; slen-1 is last character (0 indexing
	(let ((ch (string-ref s i)))
	  (cond
	   ((char=? ch #\N) (format *stdout* "   (set! y (+ y 1)) (open-trap-door! x y) (set! y (+ y 1)) (set! *step-count* (+ 1 *step-count*)) (mark-square! x y *step-count*)    ;; north ~%") (loop (+ i 1)))
	   ((char=? ch #\S) (format *stdout* "   (set! y (- y 1)) (open-trap-door! x y) (set! y (- y 1)) (set! *step-count* (+ 1 *step-count*)) (mark-square! x y *step-count*)    ;; south ~%") (loop (+ i 1)))
	   ((char=? ch #\W) (format *stdout* "   (set! x (- x 1)) (open-side-door! x y) (set! x (- x 1)) (set! *step-count* (+ 1 *step-count*)) (mark-square! x y *step-count*)    ;; west ~%") (loop (+ i 1)))
	   ((char=? ch #\E) (format *stdout* "   (set! x (+ x 1)) (open-side-door! x y) (set! x (+ x 1)) (set! *step-count* (+ 1 *step-count*)) (mark-square! x y *step-count*)    ;; east ~%") (loop (+ i 1)))
	   (#t (error (format #f "trav2-str ch=~a error" ch)))))))))
    (format *stdout* "  (make-point x y))) pts)))~%")))

			
;; given a set of points -> return a set of points through sequentially 
(define (trav2-seq t)
  (let* ((id (node-id t))
	 (sym (string->symbol (format #f "t~a" id))))
    ;; generate the actual node definitions
    (let loop ((elems (node-leafs t)))
      (cond
       ((null? elems) #f)
       (#t (let ((elem (car elems)))
	     (let ((out (trav2 elem)))
	       (loop (cdr elems)))))))
    ;; 
    (format *stdout* "~%(loading)~%")
    (format *stdout* "~%(set! ~a (lambda (pts) ~%" sym)
    (format *stdout* " ;; seq node - iterative until sequence complete  ~%" sym)
    (format *stdout* " ;; simply leave *step-count* to increment at its own volition  ~%")    
    (format *stdout* "(report)~%")    
    (let loop ((elems (node-leafs t)))
      (cond
       ((null? elems) #f)
       (#t (let ((elem (car elems)))
	     (format *stdout* " (set! pts (t~a pts))~%" (node-id elem))
	       (loop (cdr elems))))))
    (format *stdout* " pts))~%")))

(define (trav2-alt t)
  (let* ((id (node-id t))
	 (sym (string->symbol (format #f "t~a" id))))
    ;; generate the actual node definitions
    (let loop ((elems (node-leafs t)))
      (cond
       ((null? elems) #f)
       (#t (let ((elem (car elems)))
	     (let ((out (trav2 elem)))
	       (loop (cdr elems)))))))
    ;; 
    (format *stdout* "~%(loading)~%")
    (format *stdout* "~%(set! ~a (lambda (pts) ~%" sym)
    (format *stdout* " ;; alt node - union of generated points  ~%" sym)
    (format *stdout* " ;; we need to record global *step-count* and reset it to original value every alternate leg we explore ~%")    
    (format *stdout* "(report)~%")    
    (format *stdout* "(let ((new-pts (make-union (make-hash-table))) ~%" sym)
    (format *stdout* "      (original-step-count *step-count*))~%")    
    (let loop ((elems (node-leafs t)))
      (cond
       ((null? elems) #f)
       (#t (let ((elem (car elems)))
	     (format *stdout* " (set! *step-count* original-step-count)~%")
	     (format *stdout* " (set! new-pts (union-add new-pts (t~a pts)))~%" (node-id elem))
	     (loop (cdr elems))))))
    (format *stdout* " (union-points new-pts))))~%")))

;; -- need the open doors +plus+ the final end points 
;; (open-door! x y)
;; (make-union)
;; (union-points union)
;; (union-add union pts)
;; ===================================================================================
  
;; (define (go)
;;   (call-with-output-file "lisp.out"
;;     (lambda (port)
;;       (set! *stdout* port)	
;;       ;; (format port "(* type decl *)~%")
;;       ;; (format port "type tree = TSeq of tree list | TStr of string | TAlt of tree list ;; ~%")
;;       ;; (format port "(* parse tree itself - careful in is a reserved word ! *)~%")
;;       (format port "(defparameter *input* = ")  
;;       (trav input '())
;;       (format port ")~%"))))

(define *root* #f)

(define (run)
  (set! *root* (trav input))
    
  (let loop ((i 0))
    (format *stdout* "(define t~a #f)~%" i)
    (when (<= i *max-count*)
      (loop (+ i 1))))
  (trav2 *root*))


(define (go)
  (call-with-output-file "lisp2-out.scm"
    (lambda (port)
      (set! *stdout* port)
      (run)
      (set! *stdout* #t))))


;; load lisp2.scm into chicken scheme
;; (go)
;; exit to shell
;; bash build-lisp2-prog.sh
;;  .... compilation takes about 10 minutes ...








