

range: aCollection [
"find minX minY maxX maxY"
"make points a Set , whenever we add point it will check if that point has already been added"
 | x x1 x2 y y1 y2 |
"are they nil by default ?"
points := Set new. 
minX := nil .
maxX := nil .
minY := nil .
maxY := nil . 
aCollection do: [ :quad | 
	"we can declare temp variables just after start of closure "
	(quad at: 1) = $x "x=123, y=456..789  {1}=$x {2}=123 {3}=456 {4}=789"
	ifTrue: [ x := (quad at: 2).
		       y1 := (quad at: 4).
				y2 := (quad at: 5).
	  y1 to: y2 do: [ :y | points add: (x@y).
			minX = nil ifTrue:[ minX := x ].
	    	maxX = nil ifTrue:[ maxX := x ].
			minY = nil ifTrue:[ minY := y ].
			maxY = nil ifTrue:[ maxY := y ].
			
			x < minX ifTrue:[ minX := x ].
			x > maxX ifTrue:[ maxX := x ].
			y < minY ifTrue:[ minY := y ].
			y > maxY ifTrue:[ maxY := y ].
				
				"y = 6 ifTrue: [ thisContext halt ]."
				
		 ] ]
    ifFalse: [ y := (quad at: 2).
		       x1 := (quad at: 4).
				x2 := (quad at: 5).
	  x1 to: x2 do: [ :x | points add: (x@y) . 
			minX = nil ifTrue:[ minX := x ].
	    	maxX = nil ifTrue:[ maxX := x ].
			minY = nil ifTrue:[ minY := y ].
			maxY = nil ifTrue:[ maxY := y ].
	
			x < minX ifTrue:[ minX := x ].
			x > maxX ifTrue:[ maxX := x ].
			y < minY ifTrue:[ minY := y ].
			y > maxY ifTrue:[ maxY := y ].			

	     " y = 6 ifTrue: [ thisContext halt ]."
	
	 ] ]].
^ points.


