module ConsoleUi

open GameOfLife

type private PrintableRow = {y: int; cells: Cell List }

let Print board =
    let toPrintableBoard printableRows cell = 
        let isSameRow row = cell.position.y = row.y
        let row = match List.tryFind isSameRow printableRows with
                                | Some r -> r
                                | None -> {y=cell.position.y; cells=[]}

        let newRow = {row with cells=cell::row.cells }
        newRow :: (printableRows |> List.filter (not << isSameRow)) 

    let printCell cell = match cell.aliveness with Alive -> printf "X" | Dead -> printf " "

    let printRow row =
        row.cells |> List.sortBy (fun cell -> cell.position.x) |> List.iter printCell 
        printfn ""

    System.Console.Clear()
    board 
    |> List.fold toPrintableBoard []
    |> List.sortBy (fun row -> row.y)
    |> List.iter printRow