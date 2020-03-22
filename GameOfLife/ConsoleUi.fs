module ConsoleUi

open GameOfLife

let private printCell cell = match cell.aliveness with 
                                | Alive -> printf "X" 
                                | Dead -> printf " "

let print board =
    let getCellX cell = cell.position.x
    let getCellY cell = cell.position.y
    let equalsCellY y cell = getCellY cell = y
    let getRow y = board |> List.filter (equalsCellY y)

    let printCells cells =
        cells |> List.sortBy getCellX |> List.iter printCell
        printfn ""

    System.Console.Clear()
    board 
    |> List.map getCellY 
    |> Seq.distinct
    |> Seq.toList
    |> List.sort
    |> List.map getRow
    |> List.iter printCells