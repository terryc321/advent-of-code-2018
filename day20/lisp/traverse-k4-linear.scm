;; csc -O3 -o traverse traverse.scm
;;
;;
;; chicken scheme
(import (chicken format)) ;; format 

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
(define (north points) (map (lambda (pt) (let ((x (car pt))
					       (y (car (cdr pt))))
					   (list x (- y 1))))
			    points))
(define (south points) (map (lambda (pt) (let ((x (car pt))
					       (y (car (cdr pt))))
					   (list x (+ y 1))))
			    points))
(define (east points)  (map (lambda (pt) (let ((x (car pt))
					       (y (car (cdr pt))))
					   (list (+ x 1) y)))
			    points))
(define (west points) (map (lambda (pt) (let ((x (car pt))
					       (y (car (cdr pt))))
					   (list (- x 1) y)))
			    points))

;; traverse :: tree -> points -> points 

(define (trav t pts)
  (cond
   ((symbol? t)
    (cond
     ((eq? t '?) pts)
     (#t (let ((str (format #f "~a" t)))
	   (let ((len (string-length str)))
	     (let loop ((i 0)(points pts))
	       (cond
		((>= i len) points)
		(#t (let ((ch (string-ref str i)))
		      (cond
		       ((char=? ch #\N) (loop (+ i 1) (north points)))
		       ((char=? ch #\S) (loop (+ i 1) (south points)))
		       ((char=? ch #\E) (loop (+ i 1) (east points)))
		       ((char=? ch #\W) (loop (+ i 1) (west points)))
		       (#t (error (format #f "dir? {~a}" ch)))))))))))))
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
	     (format #t "processed ~a : => ~a ~%" leaf new-points)
	     (loop (cdr leafs) new-points)))))))


(define (trav-alt t pts)
  (set! alt-count (+ -1 (length t) alt-count))
  (let ((union '()))
    (let loop ((leafs (cdr t)))
      (cond
       ((null? leafs) union)
       (#t (let ((leaf (car leafs)))
	     (let ((out (trav leaf pts)))
	       ;; ??
	       (set! union (append out union))
	       (loop (cdr leafs)))))))))


(define (run)
  (trav input (list `(0 0))))


;;(trav input)
;; alt-count 2064 alternatives 
;; expectation possibly 2064 end points then no ?
;; (run)

