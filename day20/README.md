
# Day20 Aoc2018

# are we asking right questions 

are we looking for the longest possible path ?

traverse-k4-alt-count.scm there are 2054 alternative paths 

```
(expt 2 2054) 
2068288388563904467245752108074876925468422570861790978056342107361577928887545017164620890337466461676077949499001473247623651785631418950085641752732690119720376151507263749872012656024937734834947688856580324369714701993127664394357638037662474910117225334530323522744804857058628211750883353543931544673402517336938773914923838386340132447670795266260884611716979562197185676333341460105616549943307335470460390637981558870886294473747305337881836647246930382798536043821492493422410054510523604702059426725162028667842943825174591912845793610648923989474790536667726694697300382630716937484387574631107814158761984
```

so that computation is not finishing any time soon

presumably must be the longest path , 

(ALT node) ultimately will become set of choices { XXX YYY ZZZ } ... 
nest alts will be 

# well ...

parse tree is large for sure. 

tried enumerating each path , just never stops ...

furthest path ? and how many doors do need to get there . 

keep track of locations reached.

how determine furthest point if cant enumerate them all ; choose longest ?

is most of this now getting to point of guessing ? rather than reasoning ?


# Using the parse tree

"?" meaning character does not move at all
"N" north
"S" south
"W" west
"E" east

can we generate all the possible paths through the tree ?

```
(SEQ "EE" (ALT (SEQ (ALT "E" "N")) "SW") (SEQ (ALT "W" "SS")))))
 EE . E . W 
 EE . N . W 
 EE . E . SS 
 EE . N . SS 
 EE . SW . W
 EE . SW . SS 
 
```


# Parsing subproblem

continuous sequences of characters transformed into a string 

the empty string represented as "?"

removed initial "^" and final "$" as they played no further part , 
as various newlines

resultant parse

```
(SEQ "EE" (ALT (SEQ (ALT "E" "N")) "SW") (SEQ (ALT "W" "SS")))))

SEQ meaning a sequence 
"NSEW" various direrctions all concatenated to a single string
ALT meaning alternative 
"?" for the empty string to match in an alternative
```

successfully parsed input using parser written by hand in common lisp 
was beginning to doubt myself

on checking input - the closing parens are not matched correctly so i added one
also 

```
|) causes problems for parser so i added a ? extra token 
|?) so now parser can pick up ? token 
```

debugging parser was tricky and final check of the parse was if can reproduce original text
