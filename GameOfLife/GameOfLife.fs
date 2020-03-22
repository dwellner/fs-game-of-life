module GameOfLife

type Position = {x: int; y: int}
type Aliveness = Alive | Dead
type Cell = {aliveness: Aliveness; position: Position}
type Board = Cell list
type Cluster = { cell: Cell; neigbours : Cell list}

type SeedRule = int*int->Aliveness 

let private createRow y boardWidth seedRule = 
    let createCell x = { aliveness = seedRule(x,y); position = {x=x; y=y}}
    [1..boardWidth] |> List.map createCell

let NewBoard boardWidth boardHeight (seedRule: SeedRule):Board = 
    let createRow y = createRow y boardWidth seedRule
    [1..boardHeight] |> List.map createRow  |> List.reduce List.append

let private getAliveNeighbourCount cluster = 
    cluster.neigbours 
        |> List.map (fun cell -> cell.aliveness)
        |> List.sumBy(function Alive -> 1 | _ -> 0 ) 

let private isNeigbour {position=self} {position=other} =
    other.x >= self.x-1 && other.x <= self.x+1 && 
    other.y >= self.y-1 && other.y <= self.y+1 &&
    not (other.x = self.x && other.y = self.y);

//Any live cell with two or three neighbors survives.
let private shouldDie cluster = match getAliveNeighbourCount cluster with 
                                | 2 -> false
                                | 3 -> false
                                | _ -> true


//Any dead cell with three live neighbors becomes a live cell.
let private shouldSpawn cluster = getAliveNeighbourCount cluster = 3

let newState cluster = 
    let cell = cluster.cell
    match cell.aliveness with
    | Dead when shouldSpawn cluster -> {cell with aliveness=Alive} 
    | Alive when shouldDie cluster ->  {cell with aliveness=Dead}
    | _ -> cell

let Regenerate board =
    let getNeighbours cell = board |> List.filter (isNeigbour cell)
    let toCluster cell = {cell=cell; neigbours = getNeighbours cell}
    List.map (toCluster >> newState) board