
(import scheme)
(import (chicken pretty-print))
(define pp pretty-print)

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))
;; (change-directory "../day4")
;; (current-directory)



(import procedural-macros)
(import regex)

(import simple-md5)
(import simple-loops)

(import srfi-69)
;; hash-table-ref  hash key thunk
;; hash-table-set! hash key val

;; sudo chicken-install srfi-178
(import srfi-178)
;; srfi-178 provides bit-vectors


;; (import-for-syntax
;;   (only checks <<)
;;   (only bindings bind bind-case)
;;   (only procedural-macros macro-rules with-renamed-symbols once-only))

(import sequences)

(import srfi-1)

(import matchable)


;;------------------------- code -----------------------------------

;; change input file ! 
(define (get-input) (call-with-input-file "input"
		      (lambda (port)
			(read port))))

;;(define input (get-input))


(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (10+ x) (+ x 10))
(define (10- x) (- x 10))

(define rest cdr)


#|

puzzle input
464 players; last marble is worth 70918 points

circle data structure ?

0 - 1 - 2 - 3 
insertion 

list 

0 - 1 - 2 - 3 
/\ 
current
/\ 
insert
4

0 - 1 - 2 - 3 
/\
/\ 1 - 2 - 3 - 0  circle


|#

(define (run n-players last-marble debug)

  (define marbles '(0))

  (define (clockwise ys)
    (append (cdr ys) (list (first ys))))

  (define (anti-clockwise ys)
    (cons (last ys) (butlast ys)))

  #|
  ;; some test cases
  (clockwise '(1 2 3))
  (clockwise (clockwise '(1 2 3)))
  (clockwise (clockwise (clockwise '(1 2 3))))

  (anti-clockwise '(1 2 3))
  (anti-clockwise (anti-clockwise '(1 2 3)))
  (anti-clockwise (anti-clockwise (anti-clockwise '(1 2 3))))

  ;; one item is always itself
  (clockwise '(0))
  (clockwise (clockwise '(0)))
  (anti-clockwise '(0))
  (anti-clockwise (anti-clockwise '(0)))
  |#

  (define (reset)
    (set! marbles '(0)))

  ;;(define n-players 9)
  ;;(define n-players 464)
  ;;(define last-marble 25)

  (define players-score (make-vector (+ 2 n-players) 0))

  (define current-player 1)


  (define (next-player)
    (set! current-player (1+ current-player))
    (cond
     ((> current-player n-players)
      (set! current-player 1))))


  (define (insert v)
    (cond
     ((zero? (modulo v 1000))
      (format #t "v = ~a ~%" v)))
    
    (cond
     ((zero? (modulo v 23))
      ;; add marble v to players score
      (vector-set! players-score current-player (+ (vector-ref players-score current-player) v))
      ;; remove marble and add it to players score
      (do-for (r 0 7)
	      (set! marbles (anti-clockwise marbles)))
      (let ((v2 (car marbles)))
	;;(format #t "removing marble ~a ~%" v2)
	(set! marbles (cdr marbles))
	(vector-set! players-score current-player (+ (vector-ref players-score current-player) v2))))
     (#t
      (let* ((r1 (clockwise marbles))
	     (h (car r1))
	     (t (rest r1)))
	(set! marbles (clockwise (cons h (cons v t))))))))


  (define (show-marbles xs)
    (define (helper ys rs)
      (cond
       ((= (car ys) 0) (format #f "~a" (append ys (reverse rs))))
       (#t (helper (cdr ys) (cons (car ys) rs)))))
    (helper xs '()))

  (define (winner)
    (let ((m #f)
	  (winning-player #f)
	  (winning-score #f))
      (do-for (i 1 (1- (vector-length players-score)))
	      (let ((p (vector-ref players-score i)))
		(cond
		 ((not winning-score)
		  (set! winning-score p)
		  (set! winning-player 1))
		 ((> p winning-score)
		  (set! winning-score p)
		  (set! winning-player i)))))
      (format #t "winner ~a with score of ~a ~%" winning-player winning-score)))
  
  (lambda ()
    (reset)
    (let ((stop last-marble))
      (do-for (p 1 (1+ stop))
	      (insert p)
	      (cond
	       (debug (format #t "&[~a] : ~a ~%" p (show-marbles marbles))))
	      (next-player))
      (format #t "~a ~%" players-score)
      (winner))))




(define (test)
  ((run 9 25 #f))
  ((run 10 1618 #f))
  ((run 13 7999 #f))
  ((run 17 1104 #f))
  ((run 21 6111 #f))
  ((run 30 5807 #f)))
  

(define (part-1)
  ((run 464 70918 #f)))

(part-1)


#|
#(0 258813 348742 305134 309313 270740 357458 317369 292927 257839 341279 300551 289380 349717 338214 289279 275935 327768 324042 286767 283148 325883 333248 296867 277488 347709 319143 288774 260381 322482 309811 288575 277355 334185 319679 279420 370210 322104 306133 257987 349564 302518 298694 267020 348729 299469 308354 274355 357047 322067 293706 256626 338499 303220 287056 264172 336311 305918 297110 359126 335903 295623 288137 335100 330516 285795 274655 336689 316537 295458 285928 335555 325360 295404 272360 338644 316671 293158 263648 340636 294110 298948 340118 348760 299312 292624 351070 339721 297332 279175 334629 321561 272699 271180 325702 319625 283722 282385 333428 326562 293559 265860 330994 305130 276692 362662 320048 306872 285380 347049 325370 308814 269418 362088 310575 301502 258083 349655 296543 287888 259855 331219 309795 296949 266585 347342 304861 292426 356795 339312 301584 276855 340501 316315 305609 285918 340962 333434 279011 281246 338097 320227 281815 261813 315312 312421 282103 272331 334979 320774 295228 368666 342143 301830 277567 354621 319641 289598 295102 343392 341226 300409 282419 341704 331599 279729 274927 329048 318432 278379 258346 323205 305014 275958 355310 321822 316422 279294 367135 327427 308827 275397 351244 306270 285975 276515 342630 315193 304581 255945 353978 301933 291132 256688 338230 291732 282455 340777 329543 295477 293130 338662 340819 302717 280099 347416 327495 283030 273443 332239 320562 296958 282507 345135 322908 286600 265166 321890 315596 254950 366723 317063 307999 281584 348106 315483 307734 281985 352649 325448 297046 279364 347205 324404 283628 283586 336304 327927 286862 278678 334485 323489 281027 364670 328962 308081 252078 359628 302424 298323 275931 343476 317307 310334 276951 353925 314087 298761 262226 346611 307722 292040 271619 327603 309646 294375 358767 340915 297522 286815 337561 336238 281783 280854 330279 318741 294690 283343 335779 325454 292972 276687 339112 323080 281615 260461 321380 313034 291386 353812 328459 311603 269434 366429 322463 309921 268438 352328 292859 295301 263890 347651 311833 305497 281434 352200 325911 289587 269234 332940 306868 278276 357461 323608 324866 287948 367095 333184 310762 259682 362309 306487 305749 260512 354442 307872 286612 259958 329183 297879 295609 266936 348603 313639 294110 359244 330929 293144 285137 336184 327002 302918 286255 353213 335380 288533 282928 337731 319612 272452 270868 315063 317950 283217 279931 323117 323271 290899 365749 337878 309169 273817 359440 311255 305044 274480 345145 327858 311428 273223 355009 308673 289554 256385 347697 293962 292970 260692 332467 311273 295296 344674 338848 310723 285409 366482 327452 308172 266347 356864 313784 313441 270641 361008 319553 306237 267831 355953 313147 290365 251876 343572 282592 289426 252037 335064 302995 294778 350456 337875 306195 281754 344834 330567 282708 278246 335845 315780 298079 284998 340239 322800 273776 268803 334933 316031 273461 267064 311143 300109 279591 351635 325488 311850 275952 362295 327365 304539 270263 349292 312729 290463 277176 340729 313139 302251 257038 350378 305760 295040 255061 336271 290223 273304 326830 325396 291747 291483 342760 337550 309679 281806 358375 319964 290988 0) 
winner 36 with score of 370210 

real	4m55.389s
user	4m54.398s
sys	0m0.533s

|#


	






    







   
  
