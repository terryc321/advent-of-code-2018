
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

;; ---------------------------------------------------------------------
#|

current node initially false

insert 0
make new node v    #f prev-0-next #f



|#
(define (make-circular-list)
  (let ((current #f))
    (lambda (op . args)
      (cond
       ((eq? op 'insert) 
	(cond
	 ((eq? current #f)
	  (let ((arg (first args)))
	    (let ((v (list #f arg #f))) ;; v = [#f arg #f]
	      (set-car! v v)
	      (set! current v)
	      (set-car! (cdr (cdr v)) v))))
	 (#t
	  (let* ((arg (first args))
		 (v (list #f arg #f)) ;; v = [#f arg #f]
		 (prev (car current)))
	    (cond
	     ((eq? prev #f) (set-car! v v))
	     (#t (set-car! (cdr (cdr prev)) v)
		 (set-car! v prev)))
	    (set-car! (cdr (cdr v)) current)
	    (set-car! current v)
	    (set! current v)))))
       ((eq? op 'show)
	(let ((ptr current))
	  (cond
	   (ptr
	    (letrec ((advance (lambda ()
				(set! ptr (car (cdr (cdr ptr))))))
		     (iter (lambda ()
			     (format #t "~a " (car (cdr ptr)))
			     (advance)
			     (cond
			      ((eq? ptr current) 'done)
			      (#t 
			       (iter))))))
	      (format #t "[")
	      (iter)
	      (format #t "]"))))))
       ((eq? op 'remove) 
	(let ((prev (and current (car current)))
	      (next (and current (car (cdr (cdr current)))))
	      (res (and current (car (cdr current)))))
	  (cond ;; either previous does not exist or prev is current
	   ((eq? prev current) res)	    
	   ;;((eq? prev next) #f)
	   (#t (set-car! (cdr (cdr prev)) next)
	       (set! current next)
	       (set-car! next prev)
	       res))))
       ((eq? op 'current)
	(cond
	 (current (car (cdr current)))
	 (#t #f)))
       ((eq? op 'clockwise) 
	(cond
	 (current
	  (set! current (car (cdr (cdr current)))))))	 
       ((eq? op 'anti-clockwise) 
	(cond
	 (current
	  (set! current (car current)))))
       (#t (error "make-circular-list" (list 'op-not-recognised op)))))))




(define c1 (make-circular-list))


(define (test-c1) 
  (let ((c (make-circular-list)))
    (do-for (k 0 10)
	    (c 'insert k)
	    (c 'show)
	    (format #t "~%"))))

(define (test-c2) 
  (let ((c (make-circular-list)))
    (do-for (k 0 10)
	    (c 'insert k)
	    (c 'clockwise)
	    (c 'show)
	    (format #t "~%"))))



(define (test-c3) 
  (let ((c (make-circular-list)))
    (do-for (k 0 10)
	    (c 'insert k)
	    (c 'clockwise)
	    (c 'show)
	    (format #t "~%"))
    (do-for (k 0 10)
	    (c 'remove)
	    (c 'show)
	    (format #t "~%"))))


(define (test-c4) 
  (let ((c (make-circular-list)))
    (do-for (k 0 10)
	    (c 'insert k)
	    (c 'clockwise)
	    (c 'clockwise)
	    (c 'show)
	    (format #t ": ~a ~%" (c 'current)))))




(define (run n-players last-marble debug)

  (define marbles #f)

  (define (reset)
    (set! marbles (make-circular-list))
    (marbles 'insert 0))

  (define players-score (make-vector (+ 2 n-players) 0))

  (define current-player 1)

  (define (next-player)
    (set! current-player (1+ current-player))
    (cond
     ((> current-player n-players)
      (set! current-player 1))))


  (define (insert v)
    (cond
     ((zero? (modulo v 10000))
      (format #t "v = ~a ~%" v)))
    ;; feedback ??
    (cond
     ((zero? (modulo v 23))
      ;; add marble v to players score
      (vector-set! players-score current-player (+ (vector-ref players-score current-player) v))
      ;; remove marble and add it to players score
      (do-for (r 0 7)
	      (marbles 'anti-clockwise))
      (let ((v2 (marbles 'remove)))
	;;(format #t "removing marble ~a ~%" v2)
	(vector-set! players-score current-player (+ (vector-ref players-score current-player) v2))))
     (#t ;; know how to insert into circular list now
      (marbles 'clockwise)
      (marbles 'clockwise)
      (marbles 'insert v))))


  (define (show-marbles)
    (marbles 'show))

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
	       (debug 
		;;(format #t "&[~a] : ~a ~%" p (show-marbles marbles))
		(format #t "&[~a] : " p)
		(marbles 'show)
		(format #t "~%")))
	      (next-player))
      (format #t "~a ~%" players-score)
      (winner))))




(define (test)
  ((run 9 25 #t))
  ((run 10 1618 #f))
  ((run 13 7999 #f))
  ((run 17 1104 #f))
  ((run 21 6111 #f))
  ((run 30 5807 #f)))
  

(define (part-2)
  (let ((last-marble (* 100 70918))
	(n-players 464)
	(debug #f))
    ((run n-players last-marble debug))))

(part-2)



#|




#(0 3091139486 3098069518 3097512262 3087640997 3088896708 3087322742 3091148820 3088521513 3086046765 3097430368 3089601872 3092487014 3086166551 3098223179 3089847123 3090518803 3085845280 3091158735 3091908859 3084783894 3097973416 3089656807 3094957268 3085615241 3098115748 3092711052 3090218729 3091255096 3096075465 3091899398 3088934794 3084994466 3090704911 3091105064 3088555032 3087311577 3096821213 3093700256 3093655291 3085900903 3097648611 3086934000 3089921361 3079541302 3095722801 3087883346 3091527505 3094877955 3096650846 3092057968 3088434190 3099633647 3090925079 3089620542 3086690842 3090198662 3093718690 3087553197 3091524661 3088063243 3095332288 3084157099 3090197571 3091300952 3093107963 3090238678 3085673129 3094686222 3087188133 3090632282 3088237262 3092457631 3089448170 3088206949 3099913602 3091972653 3094541805 3084003063 3096825974 3086054486 3089771763 3086552705 3092830731 3092012754 3087104626 3089631674 3092300896 3095453258 3087281099 3087334277 3092315951 3088268252 3089090681 3096336309 3093828138 3091144171 3086532492 3094152232 3093693277 3090831229 3090344465 3098752205 3092167676 3090659025 3084833666 3094741933 3088398569 3090452791 3082042163 3095541203 3090829511 3092280142 3088956292 3095890052 3091151447 3085333924 3096138264 3090431378 3091586901 3087710963 3094291653 3096470263 3088247136 3094487802 3091020399 3099163576 3081919161 3089431812 3089179860 3091954173 3091479467 3088124517 3095778455 3088939686 3091047394 3081914876 3092135089 3090877222 3087038910 3097803689 3088431747 3094204904 3083872300 3098236473 3087666660 3092539052 3087585272 3095239296 3095794673 3089235966 3091681853 3091039998 3093726019 3085496519 3085114749 3093008811 3088670012 3091732908 3085144554 3095905239 3090878997 3089962314 3091803450 3092147886 3086972581 3087135397 3095998680 3093341069 3093235236 3087027030 3097639971 3090459071 3092167822 3085078137 3095971425 3089758731 3090948397 3085962754 3094150595 3092407367 3087375656 3087609340 3089871422 3094896610 3089512679 3095909648 3094450531 3088649240 3088358005 3089415792 3098023247 3083685545 3091468512 3092648655 3094773418 3091030946 3090299012 3098988601 3090339507 3089511227 3081735782 3091625929 3092681901 3088092339 3092163602 3091048155 3095443005 3083347679 3101176548 3087401139 3094334020 3087281676 3094498853 3091975059 3086991174 3088493636 3091874189 3093526062 3087363060 3087443135 3096275315 3090738811 3093880039 3085921123 3096203628 3088242199 3087722623 3082580293 3092951766 3089614068 3089408339 3098903381 3095358093 3092466995 3089129053 3095068042 3090584023 3088041466 3082748942 3093931197 3091330744 3091610988 3089266905 3094921509 3093371080 3088754375 3091084990 3092391896 3093166269 3086803010 3084279324 3091819425 3090583980 3092444234 3091269780 3097999590 3087021512 3093595738 3094645290 3094859946 3091553551 3085464189 3096248198 3086764555 3091874466 3081615697 3092983849 3092591107 3089097259 3094638315 3093897063 3097508705 3083602014 3088983878 3084439009 3092777647 3085743429 3097352806 3092923717 3091970573 3089548098 3095569643 3096151184 3089401617 3086760469 3094944893 3087699645 3091890108 3083736052 3097293832 3088133392 3091117439 3082794094 3095898876 3090861190 3091201650 3090517490 3094945968 3091572249 3087123798 3094307418 3091067994 3089684386 3085895030 3094569476 3093214505 3093457594 3091563647 3096294579 3094124614 3084750199 3087271796 3090067526 3094745899 3087571400 3087198419 3093484060 3091788924 3092025496 3086155143 3099052257 3086611733 3090203965 3092407296 3092251957 3091583565 3089923381 3097918466 3089754964 3093555680 3084426566 3095693336 3093308985 3090045441 3089615294 3091513876 3094483768 3084317009 3090020761 3086664649 3093573736 3087234700 3087632188 3097553700 3089807631 3091393041 3092888471 3093894080 3088633679 3087379948 3097120167 3088673084 3093121765 3084525494 3098723152 3090757048 3095690221 3083976775 3097666160 3088724740 3088776987 3086569065 3095435912 3091020457 3089473300 3083977378 3094224064 3090943670 3088024504 3095379330 3094156439 3089109511 3088838024 3091674546 3094865596 3087917101 3090603085 3091813336 3095509405 3090989635 3088285097 3094502315 3092437475 3088674211 3083283330 3096754535 3088012250 3090789585 3085180758 3093143301 3092868399 3086901275 3100975810 3088890769 3093716384 3081055188 3094370746 3089397835 3090825588 3092328749 3094022390 3096439980 3086222571 3091775557 3089301368 3094735075 3088985110 3086947655 3095000494 3088721071 3090141253 3084093029 3095594027 3089059253 3089115427 3096548174 3092598069 3094036795 3086445140 3098521515 3087475980 3092310201 3082733712 3095876516 3089810326 3091574811 3089437284 3096708259 3094070263 3091806676 3086219706 3094020723 3088950898 3087405963 3085940709 3096520121 3090742971 3091665122 3094846202 3095962228 3087787693 3092899529 3092614712 3095075220 3085820047 3087742410 3091111981 3093252013 3090535836 3086705445 3096397575 3088620238 3095030750 3087456468 3095298875 3093526481 3086646648 3088287514 3087280053 3094967965 3083332473 3094962172 3092388504 3090902968 0) 
winner 209 with score of 3101176548 

real	0m5.397s
user	0m5.203s
sys	0m0.189s

|#
