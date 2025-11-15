"tr '\r' '\n' < Day17Morph.st > CuisDay17Morph.st"

"does not understand new Class definition "

"
Class {
	#name : 'Day17Morph',
	#superclass : 'Morph',
	#instVars : [
		'dat'
	],
	#category : 'Advent-Of-Code-2018',
	#package : 'Advent-Of-Code-2018'
}
"

"need add a hash #click "
"Day17Morph >> #click: anEvent ["

Transcript show: 'mouse click ' ; show: anEvent ; cr . 
 ^ true
 
"]"



" !Morph subclass: #Day17Morph "
" 	instanceVariableNames: '' "
" 	classVariableNames: '' "
" 	poolDictionaries: '' "
" 	category: 'AdventCode2018'! "

!Day17Morph!
click: anEvent
 Transcript show: 'mouse click from emacs ' ; show: anEvent ; cr . 
 ^ true
! !
