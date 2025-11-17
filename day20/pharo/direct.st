
where was this package ?

AthensDemoPresenter open
" ======================================================================================== "

board := Day20GridBoardElement new
    colorCellAt: 10@10 color: Color green;      "starting room"
    colorCellAt: 10@11 color: Color yellow;
    colorCellAt: 11@10 color: Color yellow;
    colorCellAt: 12@12 color: (Color red alpha: 0.8);
    yourself.

board openInWindow titled: 'Advent of Code – Day 20 Grid'

Metacello new
    baseline: 'MyGrid';
    repository: 'tonel:///home/terry/code/advent-of-code/advent-of-code-2018/day20/pharo';
    load


" ========================================================================================= "

"Define the class (classic one-liner – works perfectly in Pharo 14)"
BlElement subclass: #Day20GridBoardElement
    instanceVariableNames: 'gridSize cellSize coloredCells'
    classVariableNames: ''
    package: 'Advent-Of-Code-2018'.

"Add all the methods"
Day20GridBoardElement compile: '
initialize
    super initialize.
    gridSize := 20 @ 20.
    cellSize := 30.
    coloredCells := Dictionary new.
    self size: gridSize * cellSize.
    self background: Color black.
    self layout: BlFrameLayout new'.

Day20GridBoardElement compile: '
rectangleForCell: aPoint
    | origin |
    origin := ((aPoint - 1) * cellSize) asFloatPoint.
    ^ Rectangle origin: origin corner: origin + cellSize'.

Day20GridBoardElement compile: '
drawOnSpartaCanvas: aCanvas
    aCanvas fill
        paint: Color black;
        path: self boundsInLocal;
        draw.

    coloredCells keysAndValuesDo: [ :pos :color |
        | rect |
        rect := self rectangleForCell: pos.
        aCanvas fill
            paint: color;
            path: rect;
            draw ].

    aCanvas stroke
        paint: (Color gray alpha: 0.25);
        strokeWidth: 0.5;
        path: [ :b |
            0 to: gridSize x do: [ :i |
                b absolute; moveTo: i*cellSize@0; lineTo: i*cellSize@self height ].
            0 to: gridSize y do: [ :i |
                b absolute; moveTo: 0@(i*cellSize); lineTo: self width@(i*cellSize) ]
        ];
        draw'.

Day20GridBoardElement compile: '
colorCellAt: gridPoint color: aColor
    coloredCells at: gridPoint put: aColor.
    self invalidate'.

Day20GridBoardElement compile: '
clearAll
    coloredCells removeAll.
    self invalidate'.
