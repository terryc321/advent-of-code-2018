
# Aoc 2018 day 17 

```
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
```

one first glance was going to pull 3 numbers from a line and be done.

on closer inspection actually need two regular expressions

tag the list as either 'x-y-y  or 'y-x-x so i know its a range of x or 
a range of y
```
x = 501 , y = A .. B

(ppcre:register-groups-bind (s1 s1 s2)
	("x=(\\d+).*y=(\\d+)\.\.(\\d+)" line :sharedp t)
	(mapcar #'parse-integer (list 'x-y-y s1 s1 s2)))


y = 13 , x = A ... B 

(ppcre:register-groups-bind (s1 s1 s2)
	("y=(\\d+).*x=(\\d+)\.\.(\\d+)" line :sharedp t)
		(mapcar #'parse-integer (list y-x-x s1 s1 s2)))


```

represent board as a hash table 
simply case of filling in and figuring out min and max values of X and Ys
convert to 2d grid 
fill it 
show grid 


## bugs : conversion from X , Y to smallX,smallY

conversion from coordinate to an array which breaks things , so 
if we go back to a hash table that will keep coordinates the same - no tricky off by 1 conversion to 
fixed sized array required 

## bug2 : unwinding from array smallX,smallY to hash table 





## development notes 

quickproject so can load packages from asd easier
(ql:quickload :quickproject)
(quickproject:make-project "foo")

(ql:quickload :aoc18)

for ppcre match one or more digits
```
use the \\d+ to mean [0-9]+

(ppcre:register-groups-bind (x1)
      ("x=(\\d+)" "x=498, y=10..13" :sharedp t)
    (list x1))
```

