
# 

Just want to cry.

lost the complete structure of project due to instability using pharo 14 development 

having packages require Bloc , which not present in any pharo distribution means 50/50 if can 
load project becasue Bloc may not load

Similarly for any other package that may or may not load.

can we debug where it is going wrong ?

how do debug pharo ?




# Reconstruction Day17 after Pharo crash

```
using file outs and git 

PetitParser

Day17.class.st - 
contains two examples small data set and own large input set 

d := Day17 new.
d range: (d puzzleInput1). 

side effects range is that sets minX maxX minY maxY points=> actual set of points 
other calls to range destroy this data - mutatable



Day17Test.class.st - petit parser test cases

```


# 

may be related to git in Pharo 

cloned bloc from github to a local repository 

Metacello new
	baseline: 'Bloc';
	repository: '/home/terry/src/Pharo/bloc/Bloc';
	load
