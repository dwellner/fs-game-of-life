// GameOfLife.fs
module GameOfLife

type Position = {x: int; y: int}
type Aliveness = Alive | Dead
type Cell = {aliveness: Aliveness; position: Position}
type Board = Cell list
type Cluster = { cell: Cell; neigbours : Cell list}

let CreateCell (x,y, seedRule) = 
    let aliveness = if seedRule(x,y) then Alive else Dead 
    { aliveness = aliveness; position = {x=x; y=y}}


let CreateRow (y, boardWidth, seedRule) = 
  [1..boardWidth] |> List.map (fun x -> CreateCell(x,y, seedRule)) 

let NewBoard (boardWidth, boardHeight, seedRule):Board = 
    let createRow y = CreateRow(y, boardWidth, seedRule)
    [1..boardHeight] |> List.map createRow  |> List.reduce List.append

let GetAliveNeighbourCount cluster = 
    cluster.neigbours 
        |> List.map (fun cell -> cell.aliveness)
        |> List.sumBy(function Alive -> 1 | _ -> 0 ) 

let IsNeigbour (self: Position, other: Position) =
    other.x >= self.x-1 && other.x <= self.x+1 && 
    other.y >= self.y-1 && other.y <= self.y+1 &&
    not (other.x = self.x && other.y = self.y);


let GetNeighbours (cell, board) = 
    board |> List.filter (fun aCell -> IsNeigbour(cell.position, aCell.position)) 

let ShouldDie cluster = 
    let aliveNeighbours = GetAliveNeighbourCount cluster 
    //Any live cell with two or three neighbors survives.
    not (List.contains aliveNeighbours [2;3])

let ShouldSpawn cluster =   
    let aliveNeighbours = GetAliveNeighbourCount cluster 
    //Any dead cell with three live neighbors becomes a live cell.
    aliveNeighbours = 3

let newState (cluster) = 
    let cell = cluster.cell 
    match cell.aliveness with
    | Dead -> if ShouldSpawn(cluster) then {cell with aliveness=Alive} else cell
    | Alive -> if ShouldDie(cluster) then  {cell with aliveness=Dead} else cell

let Regenerate board =
    let toCluster cell = {cell=cell; neigbours = GetNeighbours(cell,board)}
    List.map (toCluster >> newState) board


let Print (board:Board) =
    let {x=maxX; y=maxY} = (board |> List.rev  |> List.head) |> fun cell -> cell.position    

    System.Console.Clear()

    for y = 1 to maxY do
        for x = 1 to maxX do
            let cell = board |> List.find (fun cell -> cell.position.x = x && cell.position.y = y)
            match cell.aliveness with
            | Alive -> printf "X"
            | Dead -> printf " "
        printfn ""
    printfn ""

//Main.fs
