;; -*- geiser-scheme-implementation: guile -*-

#|

GNU Guile 3.0.10



|#


(define (find-elfs-goblins arr)
  (let loopy ((y 1))
    (when  (<= y 2)
      (let loopx ((x 1))
	(when  (<= x 3)
	  (display (list "x is " x " and y is " y))
	  (newline)
	  (loopx (+ x 1))))
      (when (<= y 2)  #t )
      (loopy (+ y 1)))))


(define (test)
  (begin
    (when #t #t)
    (when (= 1 1) #t)))


#|

 (main) ~/code/advent-code/advent-of-code-2018/day15/guile/bugs$ guile
GNU Guile UNKNOWN
Copyright (C) 1995-2024 Free Software Foundation, Inc.

Guile comes with ABSOLUTELY NO WARRANTY; for details type `,show w'.
This program is free software, and you are welcome to redistribute it
under certain conditions; type `,show c' for details.

Enter `,help' for help.
scheme@(guile-user)> (compile-file "fun6.scm")
ice-9/boot-9.scm:1712:22: In procedure raise-exception:
not found 71

Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
scheme@(guile-user) [1]> ,bt
In system/base/compile.scm:
    187:6 21 (compile-file "fun6.scm" #:output-file _ #:from _ #:to _ #:env _ #:optimization-level _ # _ #:opts _ # …)
     53:4 20 (call-with-output-file/atomic _ _ _)
In ice-9/boot-9.scm:
  1791:12 19 (with-exception-handler _ _ #:unwind? _ #:unwind-for-type _)
In system/base/compile.scm:
    69:11 18 (_)
   190:11 17 (_ #<closed: file 779066b513f0>)
   352:28 16 (read-and-compile _ #:from _ #:to _ #:env _ #:optimization-level _ #:warning-level _ #:opts _)
   265:44 15 (_ _ _)
   261:33 14 (_ #<intmap 0-98> #<module (#{ g171}#) 779066a11d20>)
In language/cps/optimize.scm:
   134:12 13 (_ _ #<module (#{ g171}#) 779066a11d20>)
    101:3 12 (optimize-higher-order-cps _ _)
In language/cps/type-fold.scm:
  1072:16 11 (type-fold _)
In language/cps/renumber.scm:
   168:36 10 (renumber #<intmap 0-4,6-23,25-36,38,43-48,50-52,54,56,59-64,66-68,70,72-73,75,80-85,87-89,91,96-101…> …)
   164:30  9 (compute-renaming _ _)
    147:7  8 (visit-fun _ _ _)
   158:19  7 (visit-fun 16 #<transient-intmap 0-4,6-15> #<transient-intmap 0-3,5-9>)
In language/cps/utils.scm:
   252:39  6 (compute-predecessors #<intmap 0-4,6-23,25-36,38,43-48,50-52,54,56,59-64,66-68,70,72-73,75,80-85,87-…> …)
    109:3  5 (compute-function-body _ _)
   131:30  4 (visit-cont _ _)
    114:9  3 (visit-cont 71 _)
In language/cps/intmap.scm:
   397:51  2 (_ _)
In ice-9/boot-9.scm:
  1712:22  1 (raise-exception _ #:continuable? _)
  1712:22  0 (raise-exception _ #:continuable? _)
scheme@(guile-user) [1]> 
scheme@(guile-user)> 
(main) ~/code/advent-code/advent-of-code-2018/day15/guile/bugs$

|#






