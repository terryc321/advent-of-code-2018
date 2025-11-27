;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

(require 'benchmark)

(benchmark-elapse 0)

(benchmark-elapse (load-file "results.el"))


;; emacs characters are prefix with ?
;; ok.?.
;; $a
;; #\a 
