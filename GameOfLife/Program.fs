let rec Tick(board) = 
   GameOfLife.Print board |> ignore
   Async.Sleep(500) |> ignore
   Tick(GameOfLife.Regenerate(board))

let random = System.Random()
Tick(GameOfLife.NewBoard(100,25, fun (x,y) -> random.Next()%2=0))