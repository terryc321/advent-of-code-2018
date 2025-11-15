Class {
	#name : 'Day17Test',
	#superclass : 'TestCase',
	#category : 'Advent-Of-Code-2018',
	#package : 'Advent-Of-Code-2018'
}

{ #category : 'tests' }
Day17Test >> testParse1 [
"all these test cases are useless unless we can install PetitParser"
	| tmp |
	tmp := Day17 new.
	tmp parse: 'x=123, y=456..789'.
	self assert: ((tmp at: 1) = $x).
	self assert: ((tmp at: 2) = 123).
	self assert: ((tmp at: 3) = $y).
	self assert: ((tmp at: 4) = 456).
	self assert: ((tmp at: 5) = 789).

	

]

{ #category : 'tests' }
Day17Test >> testParse2 [
	| tmp |
	tmp := Day17 new.
	tmp parse: 'y=123, x=456..789'.
	self assert: ((tmp at: 1) = $y).
	self assert: ((tmp at: 2) = 123).
	self assert: ((tmp at: 3) = $x).
	self assert: ((tmp at: 4) = 456).
	self assert: ((tmp at: 5) = 789).

	

]

{ #category : 'tests' }
Day17Test >> testParseLines [
	| day pLines |
	day := Day17 new.	
	pLines := day parseLines: (day dataExample1).
	self assert: ((pLines at: 1) = {$x . 495 . $y . 2 . 7 }).
	self assert: ((pLines at: 2) = {$y . 7 . $x . 495 . 501 }).
	self assert: ((pLines at: 3) = {$x . 501 . $y . 3 . 7 }).
	self assert: ((pLines at: 4) = {$x . 498 . $y . 2 . 4 }).
	self assert: ((pLines at: 5) = {$x . 506 . $y . 1 . 2 }).
	self assert: ((pLines at: 6) = {$x . 498 . $y . 10 . 13 }).
	self assert: ((pLines at: 7) = {$x . 504 . $y . 10 . 13 }).
	self assert: ((pLines at: 8) = {$y . 13 . $x . 498 . 504 }).
	
	
	
	
	

]
