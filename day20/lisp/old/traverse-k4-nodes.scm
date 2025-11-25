;; csc -O3 -o traverse traverse.scm
;;
;;
;; chicken scheme
(import (chicken format)) ;; format 
(import (chicken pretty-print)) 
(import srfi-1)
;;(import bind)
(import bindings) ;; destructuring

(define (path)
  (with-input-from-file "output/tree.lisp"
    (lambda ()
      (read))))

(define input (path))

;; may be multiple different paths
;; as one alt route may lead down multiple alt routes 
;; 

(define output-vector #f)

(define (write-string s i)
  (let ((n (+ i (string-length s))))
    n))

(define stack '())
;; (define (push! s x) (set! stack (cons x stack)))
;; (define (pop! s) (let ((top (car stack)))
;; 		(set! stack (cdr stack))
;; 		top))


;; (define (trav-alt2 alts pr k)
;;   (let loop ((alts (cdr tree)))
;;     (cond
;;      ((null? alts) (k pr))
;;      (#t (let ((alt (car alts)))
;; 	   (trav alt pr (lambda (r1)
;; 			  (loop (cdr alts)))))))))


;; (define (trav-alt tree pr k)
;;   (let ((alts (cdr tree)))
;;     (trav-alt2 alts pr k)))

;; (define (trav-alt2 alts pr k)
;;   (let loop ((alts (cdr tree)))
;;     (cond
;;      ((null? alts) (k pr))
;;      (#t (let ((alt (car alts)))
;; 	   (trav alt pr (lambda (r1)
;; 			  (loop (cdr alts)))))))))


;; (define (trav-seq tree pr k)  
;;   (let ((seqs (cdr tree)))
;;     (trav-seq2 seqs pr k)))

;; (define (trav-seq2 seqs pr k)
;;   (cond
;;    ((null? seqs) (k pr))
;;    (#t (let ((elem (car seqs)))
;; 	 (trav elem pr (lambda (r1))
;; 	       (trav2-seq (cdr seqs) r1 k))))))


;; (define (seq tree)
;;   (let loop ((elems (cdr tree)))
;;     (cond
;;      ((null? elems) nil)
;;      (#t (seq tree elems) ??))))


(define demo1 '(SEQ a (ALT (ALT b c) d ?) e))
(define demo2 '(SEQ (SEQ a (SEQ (SEQ b c d) e f) g h (SEQ i (SEQ j (SEQ k l m) o p)))))
(define demo3 '(ALT a (ALT (ALT b c) d ?) e))
(define demo4 '(ALT a (SEQ (ALT b c) d e)))
(define demo5 '(ALT a (SEQ (ALT b c))))
(define demo6 '(SEQ (ALT a b) (ALT c d) (ALT d e)))
(define demo7 '(SEQ (ALT a b) (ALT c d) (ALT d ?)))
(define demo8 '(SEQ (ALT (ALT a b c) (ALT d e f)) (ALT g h) (ALT i ?)))
(define demo9 '(ALT (ALT a) (ALT d)))
(define demo10 '(SEQ (ALT (ALT a b) (ALT c d))))
(define demo11 '(ALT ? a))
(define demo12 '(SEQ A (ALT ? a) B))

(define example4 '(SEQ ENNWSWW (ALT NEWS ?) SSSEEN (ALT WNSE ?)EE( ALT SWEN ?) NNN ))


#|
(SEQ SESESWWWWSSWNWWNENWNEENWWWWNENNNNESSESENEEEESESS (SEQ WNWWW (ALT (SEQ (ALT WWWN (ALT NEE SSE)) (SEQ (ALT N S) (SEQ (ALT WWSNEE ?)) E)) SENNNNNW) (SEQ NNNENNESENNWNEEEEEESSSWNWN (ALT (SEQ (ALT E WWSSSWSESWW) (SEQ (ALT WNENSWSE ?)) SEESSSWW (SEQ (ALT NENWESWS ?)) SEEESWSWSEEEN (SEQ (ALT W NEENWWNNW) (SEQ NEESSENNNENNENNNNW (ALT (SEQ NNWWNNNEESS (SEQ (ALT WNSE ?)) EEEEESWSSESSSENENNENEEENNWNNNWNNWSWNNNNWWSWNWSSSSSENNNESSE (ALT (SEQ (ALT NNN SWSEENESESWSWW) (SEQ N (ALT (SEQ (ALT E WWNWSWNNW) (SEQ (ALT SS WNNWSWWNNWWSSSE) (SEQ SWSWSWS (ALT (SEQ EENEE (SEQ (ALT SWEN ?)) N (SEQ (ALT NNEEWWSS ?)) (ALT W WWWSES) (SEQ (ALT ENSW ?)) SWWWWSESEE (SEQ N (ALT (SEQ (ALT W E)) SWWWSES) (SEQ (ALT ENESNWSW ?)) WWWNENWNNNE (SEQ (ALT SSEWNN ?)) NEEE (SEQ (ALT E NNNWNWSWWSEES) (SEQ (ALT ENSW ?)) WWWS (SEQ (ALT E SWWSWWWSSESESSS) (SEQ WNWN (ALT (SEQ (ALT E NWNNWWNEENWWNNNEEEESWS) (SEQ (ALT WWNEWSEE ?)) ES (SEQ (ALT W ENENE) (SEQ NWW (ALT (SEQ (ALT S NENWWSWWWWWNWSSWWSWWNENWNNESESENNENENWWSW) (SEQ WNENWNNNEENENWNWSSWWWWWWSSENEESE (ALT (SEQ SSSWNWWWSSESEE (ALT (SEQ N (SEQ (ALT WNWESE ?)) (ALT E SSSWWNN) (SEQ (ALT ESNW ?)) WWWSWNNNWSWSESSESWSSWSEENNESEENESSSENNNE (SEQ NWWWW (ALT (SEQ (ALT WNENES S)) ESSW) (SEQ SSENEESWSEENEE (ALT (SEQ SENESSWWSWWN (SEQ (ALT ENSW ?)) WSWNWNWSSESWWNWSWSESENESESSSWNWWSWNN (ALT (SEQ WSWNNWNNENWWSWSSE (ALT (SEQ SWSWWSSWNWSSSENESSESEESENESSWSWWWWWNEN (ALT (SEQ ESE (ALT (SEQ (ALT E N)) NWSWSSWWNENWWNEEN) (SEQ WWNWWNENWWWWSWWSESWSEEEESWSWSWWWSESSWSEESSWNWWSSE (ALT (SEQ EEESWSWSSWNWNWN (ALT (SEQ EE (ALT (SEQ (ALT E S)) WNNE) (SEQ (ALT S NWNENNNWNWNENWWNNENNEENWWNWWWWWNENESENNENNWWS) (SEQ (ALT E SWW) (SEQ NNE (ALT (SEQ (ALT S NNESENNEENEENWNNNWNNNNWSWSESSWSSEE) (SEQ (ALT NWES ?)) SWWWWNW (SEQ NENNW (ALT (SEQ NEE (ALT (SEQ S (ALT (SEQ (ALT E SSS)) NNENENNESSESSSSENNNNENWNEESSENNNESSSENNNESSSESSWNWWSESWWWW) (SEQ SSENEEESWWSSSSWSW (ALT (SEQ NNENNWSWNN (ALT (SEQ (ALT SSENESNWSWNN ?)) SEENENNNEN) (SEQ (ALT W ESSSSSW) (SEQ WSESSSSESENEESS (ALT (SEQ ESEE (SEQ (ALT SSWNWESENN ?)) NNENNES (ALT (SEQ EENWNENNNE (ALT (SEQ...

2 types of node
SEQ
ALT

strings either sequences of N S E W or singular ? empty epsilon value

can we build a map of the doors ?

tried traversing each route depth first search seems to be an insurmoutable 

(ALT N S) => 

(x y) encoded as single number  000300| 00050
                                    X     Y
if need a larger X Y , 


|#

(define alt-count 0)

(define *alts-max* 0)
(define *seqs-max* 0)
(define *terms-max* 0)

(define *leafs* 0)
(define *seqs* 0)
(define *alts* 0)
(define *terms* 0)



(define (north!v points len)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 1 (+ (vector-ref vf 1) 1))
	   )))))
        
(define (south!v points)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 1 (- (vector-ref vf 1) 1))
	   )))))
  
(define (east!v points)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 0 (+ (vector-ref vf 0) 1))
	   )))))
    
(define (west!v points)
  (let loop ((i 0))
    (cond
     ((>= i len) #f)
     (#t (let ((vf (vector-ref points i)))
	   (vector-set! vf 0 (- (vector-ref vf 0) 1))
	   )))))
  
  
;; traverse :: tree -> points -> points 

(define (trav t pts)
  (cond
   ((symbol? t)
    (cond
     ((eq? t '?) pts)
     (#t (let* ((str (format #f "~a" s))
		(len (string-length str))
		(v (list->vector (map (lambda (x) (list->vector x)) pts)))
		(vlen (vector-length v))
		(x 0)
		(y 0))
	   (cond
	    ((string=? str "?") "?")
	    (#t 
	     (let loop ((i 0))
	       (cond
		((>= i len) (map vector->list (vector->list v)))
		(#t
		 (let ((ch (string-ref str i)))
		   (cond
		    ((char=? ch #\N) (north! v vlen) (loop (+ i 1)))
		    ((char=? ch #\S) (south! v vlen) (loop (+ i 1)))
		    ((char=? ch #\E) (east! v vlen) (loop (+ i 1)))
		    ((char=? ch #\W) (west! v vlen) (loop (+ i 1)))
		    (#t (error "move")))))))))))))
   ((pair? t)
    (let ((op (car t)))
      (cond
       ((equal? op 'SEQ)	  
	(trav-seq t pts))
       ((equal? op 'ALT)	  
	(trav-alt t pts))
       (#t (error "unknwon")))))
   (#t (error "not symbol or pair ."))))


(define (trav-seq t pts)
  (let loop ((leafs (cdr t)) (points pts))
    (cond
     ((null? leafs) points)
     (#t (let ((leaf (car leafs)))
	   (let ((new-points (trav leaf points)))
	     ;; (format #t "processed in : ~a : => ~a ~%" pts leaf new-points)
	     (loop (cdr leafs) new-points)))))))


(define (trav-alt t pts)
  (let ((union '()))
    (let loop ((leafs (cdr t)))
      (cond
       ((null? leafs) union)
       (#t (let ((leaf (car leafs)))
	     (let ((out (trav leaf pts)))
	       (set! union (append out union))
	       (loop (cdr leafs)))))))))




(define (reset-all)
  (set! *alts* 0)
  (set! *seqs* 0)
  (set! *terms* 0)
  (set! *alts-max* 0)
  (set! *seqs-max* 0)
  (set! *terms-max* 0)
  )

(define (reset-some)
  (set! *alts* 0)
  (set! *seqs* 0)
  (set! *terms* 0)
  )


(define (run)
  (reset-all)
  (trav input (list `(0 0)))
  (reset-some)
  (trav input (list `(0 0)))
  (format #t "alts = ~a / ~a : seqs = ~a / ~a : terms = ~a / ~a ~%" *alts* *alts-max*
	  *seqs* *seqs-max*
	  *terms* *terms-max*)
  (reset-some)
  )



;;(trav input)
;; alt-count 2064 alternatives 
;; expectation possibly 2064 end points then no ?
;; (run)


;; (define (simp t)
;;   (cond
;;    ((symbol? t) t)
;;    ((and (pair? t) (= (length t) 2) (eq? (car t) 'SEQ) (alt? (second t))) (second t))
;;    ((pair? t)
;;     (let ((op (car t)))
;;       (cond
;;        ((equal? op 'SEQ)	  
;; 	(simp-seq t))
;;        ((equal? op 'ALT)	  
;; 	(simp-alt t))
;;        (#t (error "unknwon")))))
;;    (#t (error "not symbol or pair ."))))

(define (alt? x)
  (and (pair? x) (eq? (car x) 'ALT)))

(define (seq? x)
  (and (pair? x) (eq? (car x) 'SEQ)))

;; (seq (alt ..)) => just alt 
(define (simp-seq t)
  (cond
   (#t (cons 'SEQ (map simp (cdr t))))))

(define (simp-alt t)
  (cons 'ALT (map simp (cdr t))))


(define (node-count t)
  (cond
   ((symbol? t) 1)
   ((pair? t) (apply + (map node-count (cdr t))))))


;; can this tree be simplified ?
;; if we can extract individual branches
;;

;; (SEQ X) => X 

;; (SEQ (ALT WNWESE ?))  => ?

;; moves come in order - so we could try simplifying them down

;; (ALT SSWNWESENN ?) =>
;; (SEQ (ALT ESNW ?) =>  ?

;; (define (simp t)
;;   (cond
;;    ((symbol? t) t)
;;    ((and (pair? t) (= (length t) 2) (eq? (car t) 'SEQ) (alt? (second t))) (second t))
;;    ((pair? t)
;;     (let ((op (car t)))
;;       (cond
;;        ((equal? op 'SEQ)	  
;; 	(simp-seq t))
;;        ((equal? op 'ALT)	  
;; 	(simp-alt t))
;;        (#t (error "unknwon")))))
;;    (#t (error "not symbol or pair ."))))


               
(define (move s)  
  (let* ((str (format #f "~a" s))
	 (len (string-length str))
	 (x 0)
	 (y 0))
    (cond
     ((string=? str "?") "?")
     (#t 
      (let loop ((i 0))
	(cond
	 ((>= i len) #f)
	 (#t
	  (let ((ch (string-ref str i)))
	    (cond
	     ((char=? ch #\N) (set! y (+ y 1))(loop (+ i 1)))
	     ((char=? ch #\S) (set! y (- y 1))(loop (+ i 1)))
	     ((char=? ch #\E) (set! x (+ x 1))(loop (+ i 1)))
	     ((char=? ch #\W) (set! x (- x 1))(loop (+ i 1)))
	     (#t (error "move")))))))))
    (cond
     ((and (= x 0) (= y 0)) (string->symbol "?"))
     (#t (string->symbol str)))))





;;; can only simplify a child node 
;;;
;;;
;;;
(define (simp t fn)
  (cond
   ((symbol? t) t)
   ((pair? t)
    (cond
     ((seq? t) (let ((res '()))
		 (let loop ((xs (cdr t)))
		   (cond
		    ((null? xs) (cons 'SEQ (reverse res)))
		    (#t (set! res (cons (simp (fn (car xs)) fn) res))
			(loop (cdr xs)))))))
     ((alt? t) (let ((res '()))
		 (let loop ((xs (cdr t)))
		   (cond
		    ((null? xs) (cons 'ALT (reverse res)))
		    (#t (set! res (cons (simp (fn (car xs)) fn) res))
			(loop (cdr xs)))))))))))


(define (try-one in)
  (let ((alpha (simp in (lambda (expr)
			     (cond
			      ((bindable? (SEQ (ALT e ?)) expr)
			       (bind (SEQ (ALT e ?)) expr
				     (let ((mv (move e)))
				       (cond
					((eq? mv '?) (format #t "replacing ~a with ? ~%" expr) '?)
					(#t expr)))))
			      (#t expr))))))
    (format #t "node count ~a : ~%" (node-count alpha))
    alpha))


;; (ALT X (ALT Y Z))
(define (try-two in)
  (let ((alpha (simp in (lambda (expr)
			     (cond
			      ((bindable? (ALT x (ALT y z)) expr)
			       (bind (ALT x (ALT y z)) expr
				     `(ALT ,x ,y ,z)))
			      (#t expr))))))
    (format #t "node count ~a : ~%" (node-count alpha))
    alpha))



  
(define (simplify in)
  (try-two (try-one in)))


;; simplify // flatten 
;; (SEQ SESESWWWWSSWNWWNENWNEENWWWWNENNNNESSESENEEEESESS (SEQ WNWWW   => (SEQ SES... WNWWW
(define (simp-seq t)
  (cond
   ((symbol? t) t)
   ((pair? t)
    (cond
     ((seq? t) (let ((res '()))
		 (let loop ((xs (cdr t)))
		   (cond
		    ((null? xs) (cons 'SEQ res))
		    (#t
		     (let ((x (car xs)))
		       (cond
			((seq? x) (set! res (append res (map simp-seq (cdr x))))
			 (loop (cdr xs)))
			(#t
			 (set! res (append res (list (simp-seq x))))
			 (loop (cdr xs))))))))))
     ((alt? t) (cons 'ALT (map simp-seq (cdr t))))))))



;; (define (simp-alt t)
;;   (cond
;;    ((symbol? t) t)
;;    ((pair? t)
;;     (cond     
;;      ((alt? t) (let ((res '()))
;; 		 (let loop ((xs (cdr t)))
;; 		   (cond
;; 		    ((null? xs) (cons 'ALT res))
;; 		    (#t
;; 		     (let ((x (car xs)))
;; 		       (cond
;; 			((seq? x) (set! res (append res (map simp-seq (cdr x))))
;; 			 (loop (cdr xs)))
;; 			(#t
;; 			 (set! res (append res (list (simp-seq x))))
;; 			 (loop (cdr xs))))))))))
;;      ((seq? t) (cons 'SEQ (map simp-alt (cdr t))))))))






		
	       
    
	

