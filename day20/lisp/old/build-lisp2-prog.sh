#!/bin/bash

cat lisp2-header.scm > lisp2-prog.scm
cat lisp2-out.scm >> lisp2-prog.scm
cat lisp2-footer.scm >> lisp2-prog.scm

echo "compiling lisp2-prog"

csc -O3 lisp2-prog.scm -o lisp2-prog 

echo "compiled - now run lisp2-prog"

#echo "(load \"lisp2-prog.scm\")"
#rlwrap csi

#echo "(run)"




