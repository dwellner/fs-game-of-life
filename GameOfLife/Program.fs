let rec Tick board = 
   ConsoleUi.print board
   Async.Sleep 500 |> ignore
   GameOfLife.Regenerate board |> Tick

let random = System.Random()
let seedRule (_) = 
   match random.Next()%2 with 
      | 0-> GameOfLife.Alive
      | _ -> GameOfLife.Dead

GameOfLife.NewBoard 100 25 seedRule |> Tick
