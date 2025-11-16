


*** Petit Parser 2 ****

simple parser for smalltalk 

Metacello new
    baseline: 'PetitParser2';
    repository: '/home/terry/src/Pharo/petitparser2/';
    load.

Metacello new
    baseline: 'PetitParser2';
    repository: 'github://kursjan/petitparser2';
    load.


*** Split String into Lines ***

('This is line 1
This is line 2
This is line 3' splitOn: Character cr ).
 "an OrderedCollection('This is line 1' 'This is line 2' 'This is line 3')"


;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

# className .

d := Day19 new.
d parse: 'addr 1 2 3'. 
((d parse: 'addr 1 2 3') at: 1)  = 'addr'. 


d parse: 'addr 123 456 7890'. 

(#('alpha' . 'bravo' . 'charlie') includes: 'alpha') ifTrue:[1 ] ifFalse:[2]. 

Set 

(#('addr' . 'addi' . 'seti' . 'mulr' . 'eqrr' . 'gtrr') includes: 'addr' )  ifTrue: [ 1  ] ifFalse:  [ 0 ] . 

#{'addr' -> #addr . 'addi' -> #addi } associationAt: 'addr' . 

{ 1 . 2 . 3 } .  "#(1 2 3)"
d parse: 'gtrr 1 2 3'. 

[ | lines | 
	lines := (cr split: (d input)).
	lines inspect.
	1 to: (lines size) do: [ :i | 
		Transcript show: (d parse: (lines at: i)) 
		]] value. 

Character newline. 
nl className.  

(String lf)at: 1  . 

$\n split: '123\n456\n789'.
cr = ('\n' at: 1) . 
cr split: (d input). 


Character lf = line feed ?

# className .

d := Day19 new.
d parse: 'addr 1 2 3'. 
((d parse: 'addr 1 2 3') at: 1)  = 'addr'. 


d parse: 'addr 123 456 7890'. 

(#('alpha' . 'bravo' . 'charlie') includes: 'alpha') ifTrue:[1 ] ifFalse:[2]. 

Set 

(#('addr' . 'addi' . 'seti' . 'mulr' . 'eqrr' . 'gtrr') includes: 'addr' )  ifTrue: [ 1  ] ifFalse:  [ 0 ] . 

#{'addr' -> #addr . 'addi' -> #addi } associationAt: 'addr' . 

{ 1 . 2 . 3 } .  "#(1 2 3)"
d parse: 'gtrr 1 2 3'. 

" i dont get why its having such a hard time splitting a string into lines "
[ | lines | 
	lines := ((Character lf) split: (d input)).	
  Transcript show: {  'lines=' . lines  }.
	1 to: (lines size) do: [ :i | 
		 | tmp |
		tmp := d parse: (lines at: i).
		Transcript show: { i . tmp . 'teepee on my teepee' } .
		].
	'done'.
	] value. 

Character newline. 
nl className.  

(String lf)at: 1  . 

$\n split: '123\n456\n789'.
cr = ('\n' at: 1) . 
cr split: (d input). 



insanity ...


# className .

d := Day19 new.
d parse: 'addr 1 2 3'. 
((d parse: 'addr 1 2 3') at: 1)  = 'addr'. 


d parse: 'addr 123 456 7890'. 

(#('alpha' . 'bravo' . 'charlie') includes: 'alpha') ifTrue:[1 ] ifFalse:[2]. 

Set 

(#('addr' . 'addi' . 'seti' . 'mulr' . 'eqrr' . 'gtrr') includes: 'addr' )  ifTrue: [ 1  ] ifFalse:  [ 0 ] . 

#{'addr' -> #addr . 'addi' -> #addi } associationAt: 'addr' . 

{ 1 . 2 . 3 } .  "#(1 2 3)"
d parse: 'gtrr 1 2 3'. 

" i dont get why its having such a hard time splitting a string into lines "
[ | lines | 
	lines := ((Character lf) split: (d input)).	
  Transcript show: {  'lines=' . lines  }.
	1 to: (lines size) do: [ :i | 
		 | tmp |
		tmp := d parse: (lines at: i).
		Transcript show: { i . tmp . 'teepee on my teepee' } .
		].
	'done'.
	] value. 

Character newline. 
nl className.  

(String lf)at: 1  . 

$\n split: '123\n456\n789'.
cr = ('\n' at: 1) . 
cr split: (d input). 

('123\n456\n789' splitOn: Character lf) at: 1 . 
('123\n456\n789' splitOn: Character cr) at: 1 . 
(('123\n456\n789' splitOn: [ :c | c isSeparator]) at: 1) size.  "13"

('123
456
789' splitOn: (Character cr)) . 

'123\\n
456\\n
789'  



p := Day19 new.
p parse: 'addr 123 456 789'.
d data. 

#('addr' -> #addr . 'addi' -> #addi) associationAt: 'addr'. 

(Dictionary newFrom: #('addr' -> #addr . 'addi' -> #addi)) at: 'addr'.  
instance of ByteString did not understand #key

Dictionary newFrom: #('addr' -> #addr . 'addi' -> #addi).
Dictionary newFrom: #(1 -> 2 . 3 -> 4). 
Dictionary newFrom: #( #a -> 1 . #b -> 2 ). 



