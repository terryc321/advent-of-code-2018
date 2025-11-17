

"Modern fluid class definition – Pharo 14+ standard"
Day20GridBoardElement class >> define [
    <classDefinition>
    
    ^ self
        superclass: BlElement;
        instVarNames: #(gridSize cellSize coloredCells);
        package: #'Advent-Of-Code-2018'
]

