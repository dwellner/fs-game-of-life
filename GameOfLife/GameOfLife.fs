// GameOfLife.fs
module GameOfLife

type Position = {x: int; y: int}
type Aliveness = Alive | Dead
type Cell = {aliveness: Aliveness; position: Position}
type Board = Cell list
type Cluster = { cell: Cell; neigbours : Cell list}

type SeedRule = int*int->Aliveness 

let private createRow y boardWidth seedRule = 
    let createCell x = { aliveness = seedRule (x,y); position = {x=x; y=y}}
    [1..boardWidth] |> List.map createCell

let NewBoard boardWidth boardHeight (seedRule: SeedRule):Board = 
    let createRow y = createRow y boardWidth seedRule
    [1..boardHeight] |> List.map createRow  |> List.reduce List.append

let private getAliveNeighbourCount cluster = 
    cluster.neigbours 
        |> List.map (fun cell -> cell.aliveness)
        |> List.sumBy(function Alive -> 1 | _ -> 0 ) 

let private isNeigbour (self: Position) (other: Position) =
    other.x >= self.x-1 && other.x <= self.x+1 && 
    other.y >= self.y-1 && other.y <= self.y+1 &&
    not (other.x = self.x && other.y = self.y);


let private getNeighbours cell board = 
    board |> List.filter ((fun acell -> acell.position) >> isNeigbour cell.position)

let private shouldDie cluster = 
    let aliveNeighbours = getAliveNeighbourCount cluster 
    //Any live cell with two or three neighbors survives.
    not (List.contains aliveNeighbours [2;3])

let private shouldSpawn cluster =   
    let aliveNeighbours = getAliveNeighbourCount cluster 
    //Any dead cell with three live neighbors becomes a live cell.
    aliveNeighbours = 3

let newState cluster = 
    let cell = cluster.cell
    match cell.aliveness with
    | Dead when shouldSpawn cluster -> {cell with aliveness=Alive} 
    | Alive when shouldDie cluster -> {cell with aliveness=Dead}
    | _ -> cell

let Regenerate board =
    let toCluster cell = {cell=cell; neigbours = getNeighbours cell board}
    List.map (toCluster >> newState) board


type private PrintableRow = {y: int; cells: Cell List }

let Print board =
    let toPrintableBoard printableRows cell = 
        let isSameRow row = cell.position.y = row.y
        let row = match List.tryFind isSameRow printableRows with
                                | Some r -> r
                                | None -> {y=cell.position.y; cells=[]}

        let newRow = {row with cells=cell::row.cells |> List.sortBy (fun cell -> cell.position.x)}
        newRow :: (printableRows |> List.filter (isSameRow >> not)) 

    let printableBoard = board |> List.fold toPrintableBoard [] |> List.sortBy (fun row -> row.y)

    System.Console.Clear()

    for row in printableBoard do
        for cell in row.cells do
            match cell.aliveness with
            | Alive -> printf "X"
            | Dead -> printf " "
        printfn ""
    printfn ""