
# Day20 Aoc2018

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
