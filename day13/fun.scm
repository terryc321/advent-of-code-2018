

(import scheme)

(import (chicken io))
(import (chicken pretty-print))

(import (chicken format))
(import (chicken sort))
(import (chicken file))
(import (chicken process-context))

(import procedural-macros)
(import regex)

(import simple-md5)

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
		  (read-lines port))))


;; ---------------- just want a 2d grid then can process
#|


v
^ also means track is | vertical at this position

< also means track is - horizontal at this position 
>

carts are only ever on vertical tracks

+ is an intersection of track



/
\



figure out what type of corner it is
does the train stop on a corner ?

how many trains are there ?

can we visualise the solution ?

step it forward , step it backwards , speed it up n steps ?

track itself does not change

only have train positions



|#


;; 
;;(iter)


;;input ------------ puzzle ---------------------------



