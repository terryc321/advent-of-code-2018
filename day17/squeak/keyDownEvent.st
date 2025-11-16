
" scanCode not defined in squeak "
" 28 left arrow "
" "
keyDownEvent: anEvent 
	| keyCode |
	
	keyCode := anEvent keyCode . 
	({ 4 . 80 } includes: keyCode)  
	"Left arrow"
		ifTrue: [ 
			    origin := origin - (1@0).
			  Transcript show: 'left arrow pressed ' ; show: origin ; cr . 
			 ].
	({ 7 . 79 } includes: keyCode)  
	"Right arrow"
		ifTrue: [ 
			 origin := origin + (1@0).
			Transcript show: 'right arrow pressed' ; show: origin ; cr . 			    
			].
		
	({ 81 } includes: keyCode)  "down arrow"
		ifTrue:[
			 origin := origin + (0@1).
				Transcript show: 'down arrow pressed' ; show: origin ; cr . 	
			 ].

	({ 82 } includes: keyCode)  "up arrow"
		ifTrue: [ 
			 origin := origin - (0@1).
	      Transcript show: 'up arrow pressed' ; show: origin ; cr . 
			 ].
	
	({ 45 } includes: keyCode)  "minus key"
		ifTrue: [ 
	     "box size must be 1 or greater "
			 box_size := (box_size - 5) max: 20 . 			
			 ].
		
  ({ 46 } includes: keyCode)  "plus key"
		ifTrue: [ 
			"box size must be 1 or greater"
			 box_size := (box_size + 5 ) max: 20 . 
			 ].			
		
		
	"show the key that was pressed"
	"track how far left or right"	
	Transcript show: 'key keycode = ' ; show: anEvent keyCode ; cr.


