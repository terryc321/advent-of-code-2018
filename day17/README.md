
# day17 aoc2018

```
python/tidy.lisp -- common lisp file 

holds ability to determine what cup refers to what square 
what cup has a parent cup 
what cup has a child cup 
if cup is enclosed or open 
bounds of the cup 

```

once we have a language to talk about cups and know where sprinkler starts we can 
begin to investigate how cups behave given constant stream water , 
simulate it

do we always have keyword arguments ?

how do we enforce keyword arguments ? 
know ocaml allows ~f:3 to pass arguments by keyword

Theres some good stuff in the python directory
namely we got towards defining idea of a cup for the u shapes 
we could find where a brick was in big array - what cup it belonged to 
then to see if we could visualise the solution to that specific cup

a cup could also contain a smaller cup , which would influence how the cup
filled 

ran into a big crash using sbcl common lisp , 
seems a functional approach is more robust - or we may fall into
everything we try just does not work

codium service worker crashed and took somethings with it 


