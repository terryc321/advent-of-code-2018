
# Generate trap-doors side-doors data

```
lisp2.scm 
> csi
> (load "lisp2.scm")
> (go)
 this should generate lisp2-prog.scm
 
now combine lisp2-prog.scm with header and footer code using build-lisp2-prog.sh 

> time bash build-lisp2-prog.sh
... compiling lisp2-prog
> lisp2-prog
... should generate lisp2-prog-trap-doors.scm
... should generate lisp2-prog-side-doors.scm
... lisp2-prog-end-points.scm

we can now proceed with code in directory above since now we should have all the data
assume the data will take us on some sort of zig zag path as its greater than 1076 or something like that

getting good feedback anyway.


```


# Development

fun.lisp 

bar.lisp

parse1.lisp

parse2-alt.lisp - final version eventually worked out kinks 

final.lisp

# exploring the parse 

traverse.scm - see if can produce a trace of each traversal 
 chatgpt saying union for alternate clauses 
 
 
traverse-k.scm

traverse-k2.scm

traverse-k4-linear.scm  - see if we can 
