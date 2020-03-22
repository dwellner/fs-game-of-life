let rec Tick(board) = 
   GameOfLife.Print board |> ignore
   Async.Sleep(500) |> ignore
   Tick(GameOfLife.Regenerate(board))

let random = System.Random()
let seedRule (_) = 
   match random.Next()%2 with 
      | 0-> GameOfLife.Alive
      | _ -> GameOfLife.Dead

Tick(GameOfLife.NewBoard(100,25, seedRule)) 