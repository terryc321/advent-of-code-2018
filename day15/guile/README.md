
# Organisation

```
puzzle.scm - read in a grid / elfs / goblins 
```



# Creating a language to solve the problem 

guile scheme has a terrible debugging experience so we will create a language to solve a language problem.

advantages can leverage guile libraries and code , write code , also instrument code without changing 
original code as we have access to the interpreter and have unlimited freedom to change it to our will

dynamically insert watchers and remove watchers as please 
does it run interactively ?
how does emacs do this ?

```
(load "foo.scm")
```


```
;;foo.scm
(let ((cur-dir (getcwd)))
  (chdir "black")
  (load "guile.scm")
  (chdir cur-dir))
```


```
;;; intro.blk instruments evaluator to print trace : whenever a form is evaluated 
scheme@(guile-user)> (black)
New level loaded.
New level loaded.
New level loaded.
0-0: start
0-1> (load "black/examples/intro.blk")
New level loaded.
0-1: done
0-2> (load "black/examples/factorial.blk")
trace:(load "black/examples/factorial.blk")
trace:(define (factorial n) (cond ((< n 2) n) (#t (* n (factorial (- n 1))))))
trace:(lambda (n) (cond ((< n 2) n) (#t (* n (factorial (- n 1))))))
0-2: done
0-3> (factorial 100)
trace:(factorial 100)
trace:factorial
trace:100
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:#t
trace:(* n (factorial (- n 1)))
trace:*
trace:n
trace:(factorial (- n 1))
trace:factorial
trace:(- n 1)
trace:-
trace:n
trace:1
trace:(cond ((< n 2) n) (#t (* n (factorial (- n 1)))))
trace:(< n 2)
trace:<
trace:n
trace:2
trace:n
0-3: 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```


```
fun.scm -- guile solution (well if we ever get it to work that is)
```


elfs and goblins or two types characters 

each is trying to attack the others , only move horizontal or vertical

look for shortest path to enemy , shortest path to any desired location is taken same as move criteria below.

first to move is taken top left grid to last at bottom right , moving horizontally

need to think out strategy of code organisation 

draw a diagram of how it should work

think about making a debugging environment as guile scheme is nonsensical 

think about an abstraction layer than if go below - just assume computer is on fire and walk away !



# Graphical Ability

install the shared library to correct location , without this everything else will not work

```
> sudo cp -v pixelformat/libpixelformat.so /opt/guile/lib/guile/3.0/extensions
```

add the directory holds this file into guile load path

```
> guile
> (add-to-load-path "/home/terry/code/advent-code/advent-of-code-2018/day15/guile")
```

with the load path updated guile can now work with these instructions

```
(use-modules (macros fcase))
(use-modules (macros inc))
(use-modules ((graphics sdl2 sdl) #:prefix sdl:))
(use-modules ((graphics sdl2 image) #:prefix img:))
(use-modules ((graphics cairo cairo) #:prefix cairo:))
```

guile will look for fcase.scm inside macros directory on the load path 

```
(use-modules (macros fcase))
```

similarly for increment macros , really simple incf version from common lisp 

```
(use-modules (macros inc))
```


the shared library libpixelformat.so is a little more complex . it needs to be compiled and installed in
correct location
```
> sudo cp -v pixelformat/libpixelformat.so /opt/guile/lib/guile/3.0/extensions
```

this is because my guile is installed at
```
/opt/guile
/opt/guile/bin/guile
/opt/guile/lib/guile/3.0/extensions <-- where shared libraries should go
/opt/guile/share/guile/site/3.0 <--     where guile scheme code should go make up the modules
```


try use a guile SDL + CAIRO library to see if we can do something graphical 

model 

walls , caverns , elfs , goblins 

simple 2d grid 

smalltalk 

# THE BOOK on programming 

# SETUP guile scheme 

emacs <= geiser SETUP

guile binary installed in /opt/guile/bin/guile 

guile shared libraries go into ? 

libpixelformat is shared library that does little C magic.

guile module libraries go into ? 

can we fool emacs into loading our own "EXECUTABLE" which is really just a shell script instead of actual guile executable

this allow us to intevene and set required envrionemtn both for loading shared libraries , and guile itself



# aoc 2018 day 15 


