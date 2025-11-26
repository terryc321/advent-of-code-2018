
# Attempt 4

really only need lisp2.scm and the input tree.lisp

traverse the tree and figure out what are the open doors up down ,
open door left right , open space where we land

```
# use chicken to compile a lisp2 executable
make

# this will run the lisp2 executable
make run

# produces results.scm format list of (x y ch)
# #\. empty space
# #\_ trap door - can move vertically north south
# #\| side door - can move laterally east west
# #\X start location
# 


```
