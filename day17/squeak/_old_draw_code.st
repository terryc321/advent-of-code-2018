
 " draw some water - for fun " 
water ifNotNil: [ 
	playback ifTrue: [ 
		1 to: (step min: water size) do: [ :i | 
			| pt |
			pt := water at: i .
          self plotPoint: pt onCanvas: aCanvas withColor: (Color blue).					
		]. ]
	ifFalse: [
water do: [ :pt | 
		self plotPoint: pt onCanvas: aCanvas withColor: (Color blue).
	].
].
].

fake ifNotNil: [ 
	playback ifTrue: [
		]
	ifFalse: [
	fake do: [ :pt | 
		self plotPoint: pt onCanvas: aCanvas withColor: (Color orange).
	]. ].
].


