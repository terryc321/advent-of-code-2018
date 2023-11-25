

(import scheme)
(import (chicken pretty-print))
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

(define input (call-with-input-file "input"
		(lambda (port)
		  (read port))))

#|

[1518-03-10 23:58] Guard #2777 begins shift
[1518-04-26 00:50] falls asleep
[1518-09-20 00:39] wakes up

[ year-month-day 24hr clock ] 
                              Guard #-integer- begins shift
                              falls asleep
                              wakes up

|#


(define precomp (regexp "\\[a\\]"))

(string-match precomp "[a]")
(string-match-positions precomp "[a]")

;;(define precomp2 (regexp "\[([0-9]+)\-([0-9]+)\-([0-9+])"))
;(define precomp2 (regexp "^\\[([0-9]+).*"))
(define pre-start (regexp "^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]{2}):([0-9]{2})\\] Guard #([0-9]+) begins shift$"))
(define tstr2 "[1518-05-24 23:56] Guard #1721 begins shift")

(string-match pre-start tstr2)
(string-match-positions pre-start tstr2)

(define pre-wake (regexp "^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]{2}):([0-9]{2})\\] wakes up$"))
(define tstr3 "[1518-05-19 00:53] wakes up")
(string-match pre-wake tstr3)

(define pre-sleep (regexp "^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]{2}):([0-9]{2})\\] falls asleep$"))
(define tstr4 "[1518-09-14 00:15] falls asleep")
(string-match pre-sleep tstr4)


;; unsorted is the parsed input
(define (unsorted)
  (let ((res '()))
    (do-list (str input)
	     (let ((m1 (string-match pre-start str))
		   (m2 (string-match pre-wake str))
		   (m3 (string-match pre-sleep str)))
	       (cond
		(m1 ;;(format #t "~a~%" (cdr m1))
		    (set! res (cons (append (map string->number (cdr m1)) '(guard-begins-shift)) res)))
		(m2 ;;(format #t "~a~%" (cdr m2))
		    (set! res (cons (append (map string->number (cdr m2)) '(wakes-up)) res)))
		(m3 ;;(format #t "~a~%" (cdr m3))
		    (set! res (cons (append (map string->number (cdr m3)) '(falls-asleep)) res)))
		(#t (error "unsorted" (list str))))))
    res))
#|
is this not how to make a sort algorithm ?
|#
;;;;; buggy sort ?? or cal-sort buggy ??
(define (cal-sort x y)
    (and
     (< (first x) (first y))
     (< (second x) (second y))
     (< (third x) (third y))
     (< (fourth x) (fourth y))
     (< (fifth x) (fifth y))))
 
(sort '((1518 3 4 0 1 2879) (1518 3 5 0 41) (1518 4 1 0 1 1049) (1518 5 11 0 5) (1518 4 6 0 12))
      cal-sort)
;; output from above 
;; ((1518 3 4 0 1 2879) (1518 3 5 0 41) (1518 4 1 0 1 1049) (1518 5 11 0 5) (1518 4 6 0 12))
;;                                                                ^^^ -- in wrong place  ^^     

(cal-sort '(1518 3 4 0 1 2879)  '(1518 3 3 11 59))

(cal-sort '(1518 3 4 0 1 2879)  '(1518 3 5 0 41))
(cal-sort  '(1518 5 19 0 53) '(1518 4 19 23 48 773))



(define (sorted)
  (sort (unsorted) cal-sort))


#|

this is weird , think if it sorted it based on month then months should all be in order no ?

(match '(1 2 3 4 5 6)
 ((a b c d e . _) (format #t "matched ~a ~a ~a ~a ~a ~%" a b c d e)))

(match '(1 2 3 4 5)
 ((a b c d e . _) (format #t "still matched ~a ~a ~a ~a ~a ~%" a b c d e)))

why write own insertion sort ?

|#

#|
;; sort algorithm one	    
(define (fix)
  (let ((data (unsorted))
	(res '()))
    (letrec ((foo (lambda (ys)
		    (cond
		     ((null? ys) res)
		     (#t #f
			 (foo (cdr ys)))))))
      (set! res (list (car data)))
      (set! data (cdr data))
      (do-list (d data)
	       (foo 

      (foo data))))

|#


#|
some data

((1518 9 20 0 39) (1518 10 7 0 55) (1518 7 7 0 12) (1518 4 4 0 47) (1518 4 26 0 50) (1518 3 10 23 58 2777) (1518 5 21 0 52) (1518 8 20 0 42) (1518 5 20 0 33) (1518 7 29 23 56 2141) (1518 4 18 0 21) (1518 3 20 0 0 2777) (1518 10 3 0 
|#


#|

 (1518 3 13 0 1 769 guard-begins-shift)
 (1518 3 13 0 24 falls-asleep)
 (1518 3 13 0 50 wakes-up)
 (1518 3 13 0 55 falls-asleep)
 (1518 3 13 0 58 wakes-up)
 (1518 3 13 23 59 1907 guard-begins-shift) <<

|#
(define (score x)
  (match x 
    ((year month day hour minute . _)
     ;;(let ((hour (if (= hour 0) 24 hour)))
       (+ minute (* hour 60) (* day 25 60) (* month 25 60 100)))
    ((_) (error "score" (list 'no-match)))))

(define (sorted)
  (let ((foo (lambda (x y)
	       (< (score x) (score y)))))
    (sort (unsorted) foo)))

#|
how many guards are there ?
|#
(define (how-many-guards)
  (let ((data (sorted))
	(h (make-hash-table))
	(guard-list '())
	(n-guards 0))
    (do-list (info data)
	     (cond
	      ((= (length info) 7)
	       (let* ((guard-id (sixth info))
		      (val (hash-table-ref/default h guard-id #f)))
		 (cond
		  (val #f)
		  (#t (hash-table-set! h guard-id #t)
		      (set! n-guards (+ 1 n-guards))
		      (set! guard-list (cons guard-id guard-list))))))))
    (values (sort guard-list <)
	    n-guards)))

#|
#;1652> (how-many-guards)
(97 191 683 769 773 991 1049 1187 1213 1277 1381 1493 1721 1907 2113 2141 2711 2713 2749 2777 2879 2917)
22

so now know 22 guards
|#

;; this will hold the times the guard is awake or asleep on a particular day
;; indexed by guard id
(define guard-hash #f)


#|
when shift change occurs the previous guard is always awake
because concentrating on when guard is asleep

|#		 
(define (process)
  (let ((data (sorted))
	(state 'awake)
	(mins '())
	(current-info #f)
	(current-guard #f))
    (letrec ((iterate (lambda (ys)
			(cond
			 ((null? ys) (change-guard '(0 0 0 0 0 0 0))) ;; force dummy guard change
			 (#t (let* ((info (car ys))
				    (op (last info)))
			       (cond
				((eq? op 'guard-begins-shift) (change-guard info))
				((eq? op 'falls-asleep) (falls-asleep (fifth info)))
				((eq? op 'wakes-up) (wakes-up (fifth info)))
				(#t (error "process" (list op))))
			       (iterate (cdr ys)))))))
	     (change-guard (lambda (new-info)
			     (cond ;; process previous guard if exists
			      (current-guard
			       (assert state 'awake)		
			       ;;(format #t "id ~a : mins ......... ~a ......... ~%" current-guard (reverse mins))
			       (set! mins (reverse mins))
			       ;;(format #t "~%")
			       (fancy-viz mins current-guard current-info)
			       ;;(format #t " : id ~a " current-guard)
			       ))
			     ;; make ready for new guard
			     (set! current-guard (sixth new-info))
			     (set! current-info new-info)
			     (set! mins '())			     
			     (set! state 'awake)
			     #t))
	     (wakes-up (lambda (minute)
			 (set! state 'awake)
			 (set! mins (cons minute mins))
			 #t))
	     (falls-asleep (lambda (minute)
			     (set! state 'asleep)
			     (set! mins (cons minute mins))
			     #t))
	     )
      (iterate data))))





#|
given an even finite list of numbers
 (asleep wakes alseep wakes ...)
|#
(define (fancy-viz xs guard-id guard-info)
  (let ((state 'awake)
	(minute-vector (make-vector 60 0)))
    (letrec ((show (lambda (min)
		     (cond
		      ((eq? state 'awake) 
		       ;;(format #t ".")
		       )
		      (#t 
		       (vector-set! minute-vector min 1)
		       ;;(format #t "#")
		       ))))
	     (foo (lambda (ys min)
		    (cond
		     ((> min 59) #t)
		     ((null? ys) 
		      (show min)
		      (foo ys (+ min 1)))
		     (#t (let ((t (car ys)))
			   (cond
			    ((and (eq? state 'awake) (= t min))
			     (set! state 'asleep)
			     (show min)
			     (foo (cdr ys) (+ min 1))
			     )
			    ((and (eq? state 'asleep) (= t min))
			     (set! state 'awake)
			     (show min)
			     (foo (cdr ys) (+ min 1))
			     )
			    (#t 
			     (show min)
			     (foo ys (+ min 1))))))))))
      ;;(format #t "xs : ~a ~%" xs)
      (foo xs 0)
      (record-guard-hash minute-vector guard-id guard-info)
      minute-vector)))

(define (reset)
  (set! guard-hash (make-hash-table))
)


(define (record-guard-hash mvec id info)
  (let ((val (hash-table-ref/default guard-hash id #f)))
    (cond
     (val  (hash-table-set! guard-hash id (cons (list mvec info) val)))
     (#t (hash-table-set! guard-hash id (list (list mvec info))))))
)



(define (show-mvec mvec)
  (do-for (k 0 60)
	  (let ((val (vector-ref mvec k)))
	    (cond
	     ((zero? val) (format #t "."))
	     (#t (format #t "#"))))))

(define (pad n)
  (let ((s (format #f "~a" n)))
    (cond
     ((< (string-length s) 2) (string-append "0" s))
     (#t s))))


(define (show-date info)
  (match info
    ((year month day hour min id . _)
     (format #t "~%~a-~a-~a ~a:~a @ ~a : " year (pad month) (pad day) (pad hour) (pad min) id))
    (_ (error "show-date" (list info)))))


(define (show-guard-info mv-info)
  (show-date (second mv-info))
  (show-mvec (first mv-info)))


(define (part-1)
  (reset)
  (process)
  (let ((res '()))
  (do-list (guard-id (how-many-guards))
	   (let ((val (hash-table-ref guard-hash guard-id)))
	     (format #t "~%")
	     (map show-guard-info (reverse val))
	     (let ((mvecs (list->vector (map first (reverse val)))))
	       (let ((n-mvecs (vector-length mvecs))
		     (max-t 0)
		     (max-min 0)
		     (tot-t 0)
		     (sum-tot 0))
		 (do-for (t 0 60)
			 (set! tot-t 0)
			 (do-for (v 0 n-mvecs)
				 (let* ((vec (vector-ref mvecs v))
					(state (vector-ref vec t)))
				   (cond
				    ((= state 0) #f)
				    ((= state 1) (set! sum-tot (+ sum-tot 1))
				     (set! tot-t (+ 1 tot-t))
				     (cond
				      ((> tot-t max-t)
				       (set! max-t tot-t)
				       (set! max-min t))))))))
		 (format #t "~%max-min ~a : max-t ~a : sum-tot ~a ~%"
			 max-min 
			 max-t
			 sum-tot)
		 (set! res (cons `((sum ,sum-tot)(min ,max-min)(id ,guard-id)) res))
		 ))))
  res))





(define (sum-sort x y)
  (> (second (assoc 'sum x))
     (second (assoc 'sum y))))

(define (asleep)
  '(((sum 578) (min 37) (id 1049))
    ((sum 501) (min 35) (id 769))
    ((sum 497) (min 35) (id 2917))
    ((sum 488) (min 49) (id 2879))
    ((sum 410) (min 33) (id 1277))
    ((sum 362) (min 51) (id 2777))
    ((sum 355) (min 50) (id 2711))
    ((sum 321) (min 45) (id 2749))
    ((sum 320) (min 52) (id 1907))
    ((sum 300) (min 33) (id 1721))
    ((sum 299) (min 18) (id 1493))
    ((sum 260) (min 42) (id 2141))
    ((sum 260) (min 38) (id 1213))
    ((sum 257) (min 30) (id 191))
    ((sum 254) (min 22) (id 683))
    ((sum 214) (min 24) (id 2713))
    ((sum 171) (min 15) (id 773))
    ((sum 168) (min 18) (id 97))
    ((sum 163) (min 25) (id 991))
    ((sum 139) (min 39) (id 1187))
    ((sum 0) (min 0) (id 2113))
    ((sum 0) (min 0) (id 1381))))

#|
 guards 2113 and 1381 never slept
 guard 1049 slept most with 578 mins , most likely asleep at min 37
  (* 37 1049)
 38813

|#


#|
unsorted guard time asleep , min most asleep and id
|#


#|

find which guard sleeps the most ?
then with that guard , what minute does jessie sleep most ?
jessie id ?
what is jessie id multiplied by minute slept most ?

|#


(define (part-2)
  (reset)
  (process)
  (let ((res '()))
  (do-list (guard-id (how-many-guards))
	   (let ((val (hash-table-ref guard-hash guard-id)))
	     (format #t "~%")
	     (map show-guard-info (reverse val))
	     (let ((mvecs (list->vector (map first (reverse val)))))
	       (let ((n-mvecs (vector-length mvecs))
		     (max-t 0)
		     (max-min 0)
		     (tot-awake 0)
		     (tot-asleep 0)
		     (freq 0)		     
		     (max-freq 0))
		 (do-for (t 0 60)
			 (set! tot-awake 0)
			 (set! tot-asleep 0)
			 (set! freq 0)
			 (do-for (v 0 n-mvecs)
				 (let* ((vec (vector-ref mvecs v))
					(state (vector-ref vec t)))
				   (cond
				    ((= state 0) (set! tot-awake (+ 1 tot-awake)))
				    ((= state 1) (set! tot-asleep (+ 1 tot-asleep)))
				    (#t (error "part-2" (list 'state-expected-1-or-0))))))
			 ;; BUG ?? not looking at percentage asleep to awake - this solution rejected..
			 ;;(set! freq (/ tot-asleep (+ tot-asleep tot-awake)))
			 (set! freq tot-asleep)
			 (cond
			  ((> freq max-freq)
			   (set! max-freq freq)
			   (set! max-min t))))
			  
		 ;; (format #t "~%max-min ~a : max-freq ~a : sum-tot ~a ~%"
		 ;; 	 max-min 
		 ;; 	 max-freq
		 ;; 	 sum-tot)
		 (set! res (cons `((freq ,max-freq)(min ,max-min)(id ,guard-id)) res))
		 ))))
  res))


(define (freq-sort x y)
  (> (second (assoc 'freq x))
     (second (assoc 'freq y))))


#|
(define (frequencies)
  '(((freq 14/15) (min 45) (id 2749))
    ((freq 12/13) (min 42) (id 2141))
    ((freq 21/23) (min 49) (id 2879))
    ((freq 7/8) (min 22) (id 683))
    ((freq 6/7) (min 24) (id 2713))
    ((freq 17/20) (min 37) (id 1049))
    ((freq 14/17) (min 33) (id 1277))
    ((freq 4/5) (min 35) (id 2917))
    ((freq 3/4) (min 51) (id 2777))
    ((freq 3/4) (min 33) (id 1721))
    ((freq 14/19) (min 35) (id 769))
    ((freq 5/7) (min 52) (id 1907))
    ((freq 5/7) (min 38) (id 1213))
    ((freq 5/7) (min 25) (id 991))
    ((freq 11/16) (min 50) (id 2711))
    ((freq 2/3) (min 18) (id 1493))
    ((freq 5/8) (min 39) (id 1187))
    ((freq 5/8) (min 18) (id 97))
    ((freq 4/7) (min 15) (id 773))
    ((freq 1/2) (min 30) (id 191))
    ((freq 0) (min 0) (id 2113))
    ((freq 0) (min 0) (id 1381))))
|#			  

#|

1st solution rejected , not this type of frequency , not a percentage apparently

(define (solution-part-2)
  #|
  1518-02-21 00:00 @ 2749 : .........########################################...........
  1518-02-21 23:58 @ 2749 : ................................................###########.
  1518-02-23 00:01 @ 2749 : ...............................#################....#.......
  1518-03-08 00:00 @ 2749 : .............................##########################.....
  1518-03-18 00:00 @ 2749 : ..........................................#####.............
  1518-03-26 00:00 @ 2749 : .........##################..........###########...#######..
  1518-04-06 23:59 @ 2749 : .........................##########.......############......
  1518-05-19 00:02 @ 2749 : ....................................##########...####.......
  1518-06-11 23:56 @ 2749 : .............................########################.......
  1518-07-24 00:02 @ 2749 : ..........#################################################.
  1518-09-17 00:00 @ 2749 : ...........#####################################............
  1518-10-01 00:00 @ 2749 : .......................................##...####............
  1518-10-06 00:00 @ 2749 : .............................................#..............
  1518-10-08 23:56 @ 2749 : ...........................................####.............
  1518-10-26 23:59 @ 2749 : ..................#####.....##################...#####......
  max-min 45 : max-t 14 : sum-tot 321                                    ^--- 14/15 freq asleep
  |#
  (let ((guard-id 2749)
	(min-chosen 45))
    (* guard-id min-chosen)))

|#
		     
#|
maybe which guard served most and was asleep the most , total number of times found asleep

|#		     
		      
(define (frequencies2)
  '(((freq 21) (min 49) (id 2879))
    ((freq 17) (min 37) (id 1049))
    ((freq 16) (min 35) (id 2917))
    ((freq 14) (min 45) (id 2749))
    ((freq 14) (min 33) (id 1277))
    ((freq 14) (min 35) (id 769))
    ((freq 12) (min 51) (id 2777))
    ((freq 12) (min 42) (id 2141))
    ((freq 11) (min 50) (id 2711))
    ((freq 10) (min 52) (id 1907))
    ((freq 10) (min 38) (id 1213))
    ((freq 9) (min 33) (id 1721))
    ((freq 8) (min 18) (id 1493))
    ((freq 8) (min 30) (id 191))
    ((freq 7) (min 22) (id 683))
    ((freq 6) (min 24) (id 2713))
    ((freq 5) (min 39) (id 1187))
    ((freq 5) (min 25) (id 991))
    ((freq 5) (min 18) (id 97))
    ((freq 4) (min 15) (id 773))
    ((freq 0) (min 0) (id 2113))
    ((freq 0) (min 0) (id 1381))))

(define (try-part-2-again)
  (let ((guard-id 2879)
	(min-chosen 49))
    (* guard-id
       min-chosen)))

#|
#;10198> (try-part-2-again)
141071

accepted...

|#
















      
    
























