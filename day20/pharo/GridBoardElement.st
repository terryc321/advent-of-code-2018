Class {
    #name : 'Day20GridBoardElement',
    #superclass : 'BlElement',
    #instVars : [
        'gridSize',
        'cellSize',
        'coloredCells'
    ],
    #category : 'Advent-Of-Code-2018',
    #package : 'Advent-Of-Code-2018'
}

{ #category : 'initialization' }
Day20GridBoardElement >> initialize [
    super initialize.
    
    gridSize := 20 @ 20.          "change as needed for your puzzle"
    cellSize := 30.
    coloredCells := Dictionary new.
    
    self size: gridSize * cellSize.
    self background: Color black.
    self layout: BlFrameLayout new
]

{ #category : 'accessing' }
Day20GridBoardElement >> cellSize: anInteger [
    cellSize := anInteger.
    self size: gridSize * cellSize.
    self invalidate
]

{ #category : 'accessing' }
Day20GridBoardElement >> gridSize: aPoint [
    gridSize := aPoint.
    self size: gridSize * cellSize.
    self invalidate
]

{ #category : 'private' }
Day20GridBoardElement >> rectangleForCell: aPoint [
    "aPoint is 1-based grid coordinates (1@1 is top-left)"
    | origin |
    origin := ((aPoint - 1) * cellSize) asFloatPoint.
    ^ Rectangle origin: origin corner: origin + cellSize
]

{ #category : 'drawing' }
Day20GridBoardElement >> drawOnSpartaCanvas: aCanvas [

    "Background"
    aCanvas fill
        paint: Color black;
        path: self boundsInLocal;
        draw.

    "Colored cells (e.g. rooms, doors, etc.)"
    coloredCells keysAndValuesDo: [ :pos :color |
        | rect |
        rect := self rectangleForCell: pos.
        aCanvas fill
            paint: color;
            path: rect;
            draw
    ].

    "Grid lines – subtle"
    aCanvas stroke
        paint: (Color gray alpha: 0.25);
        strokeWidth: 0.5;
        path: [ :b |
            0 to: gridSize x do: [ :i |
                b absolute; moveTo: i * cellSize @ 0; lineTo: i * cellSize @ self height ].
            0 to: gridSize y do: [ :i |
                b absolute; moveTo: 0 @ (i * cellSize); lineTo: self width @ (i * cellSize) ]
        ];
        draw
]

{ #category : 'api' }
Day20GridBoardElement >> colorCellAt: gridPoint color: aColor [
    coloredCells at: gridPoint put: aColor.
    self invalidate
]

{ #category : 'api' }
Day20GridBoardElement >> clearCellAt: gridPoint [
    coloredCells removeKey: gridPoint ifAbsent: [ ].
    self invalidate
]

{ #category : 'api' }
Day20GridBoardElement >> clearAll [
    coloredCells removeAll.
    self invalidate
]
