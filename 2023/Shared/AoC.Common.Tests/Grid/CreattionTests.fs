module AoC.Common.Tests.Grid.CreattionTests

open System
open AoC.Common
open Xunit


[<Fact>]
let ``zero rows is a failure`` () =
    //let a = Action()
    Assert.Throws<ArgumentOutOfRangeException>(
        (fun () -> Grid.create 0 10 "13" |> ignore)
    ) |>ignore
    ()

[<Fact>]
let ``zero columns is a failure`` () =
    //let a = Action()
    Assert.Throws<ArgumentOutOfRangeException>(
        (fun () -> Grid.create 10 0 "13" |> ignore)
    ) |>ignore
    ()

[<Fact>]
let ``result has correct row count`` () =
    let g: int Grid.Grid = Grid.create 16 5 13
    Assert.Equal(16, Grid.rowCount g)
    ()

[<Fact>]
let ``result has correct column count`` () =
    let g: int grid = Grid.create 16 5 13
    Assert.Equal(5, Grid.colCount g)
    ()

[<Fact>]
let ``result has correct value in all cells`` () =
    let v = "abc"
    let g = Grid.create 16 5 v
    Assert.Equal(16, Grid.rowCount g)
    ()
