module GameOfLife.Tests

open GameOfLife
open Xunit

let AliveCell y = {aliveness= Alive; position={x=1; y=y}}
let DeadCell y = {aliveness= Dead; position={x=1; y=y}}

[<Fact>]
let ``Alive cell should die when more than three alive neigbours`` () =
    let newState = newState({
        cell=AliveCell(1); 
        neigbours = [AliveCell(2);AliveCell(3);AliveCell(4);AliveCell(5)]
    })
    Assert.Equal(Dead, newState.aliveness)

[<Fact>]
let ``Alive cell should stay alive when three alive neigbours`` () =
    let newState = newState({
        cell=AliveCell(1); 
        neigbours = [AliveCell(2);AliveCell(3);DeadCell(4);AliveCell(5)]    
    })
    Assert.Equal(Alive, newState.aliveness)

[<Fact>]
let ``Alive cell should stay alive when two alive neigbours`` () =
    let newState = newState({
        cell=AliveCell(1); 
        neigbours = [DeadCell(2);AliveCell(3);AliveCell(4);DeadCell(5)
    ]})
    Assert.Equal(Alive, newState.aliveness)

[<Fact>]
let ``Alive cell should die when less than two alive neigbours`` () =
    let newState = newState({
        cell=AliveCell(1); 
        neigbours = [DeadCell(2);DeadCell(3);AliveCell(4);DeadCell(5)
    ]})
    Assert.Equal(Dead, newState.aliveness)
