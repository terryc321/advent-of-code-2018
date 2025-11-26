
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
 
 (main) ~/code/advent-code/advent-of-code-2018/day20/lisp/old$ time bash build-lisp2-prog.sh
compiling lisp2-prog
(don't worry - still compiling...)  << this is actually a thing in chicken scheme ...
compiled - now run lisp2-prog

real	8m14.091s  .......... 8 minutes compiling ? ... i think it got stuck in a rut ....
user	8m11.305s
sys	0m2.679s
 

> lisp2-prog
... should generate lisp2-prog-trap-doors.scm
... should generate lisp2-prog-side-doors.scm
... lisp2-prog-end-points.scm

running node 4346 / 4352 
running node 4347 / 4352 
running node 4348 / 4352 
running node 4349 / 4352 
running node 4350 / 4352 
running node 4351 / 4352 
running node 4352 / 4352 

Error: unbound variable: *doors*  ........ ouch ! lets fix it !

        Call history:

        lisp2-prog.scm:64: loop   
        lisp2-prog.scm:60: srfi-69#hash-table-ref/default         
        lisp2-prog.scm:64: loop   
        lisp2-prog.scm:60: srfi-69#hash-table-ref/default         
        lisp2-prog.scm:64: loop   
        lisp2-prog.scm:60: srfi-69#hash-table-ref/default         
        lisp2-prog.scm:64: loop   
        lisp2-prog.scm:60: srfi-69#hash-table-ref/default         
        lisp2-prog.scm:64: loop   
        lisp2-prog.scm:60: srfi-69#hash-table-ref/default         
        lisp2-prog.scm:64: loop   
        lisp2-prog.scm:53942: union-points        
        lisp2-prog.scm:68: srfi-69#hash-table-map         
        lisp2-prog.scm:53976: scheme#call-with-output-file        
        lisp2-prog.scm:53978: chicken.format#format       
        lisp2-prog.scm:53979: srfi-69#hash-table-for-each               <--

real    34m39.376s
user    34m22.948s
sys     0m16.127s


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
