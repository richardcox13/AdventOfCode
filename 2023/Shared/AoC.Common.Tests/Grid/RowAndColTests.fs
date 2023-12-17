module AoC.Common.Tests.Grid.RowAndColTests

open System
open AoC.Common
open Xunit

[<Fact>]
let ``colCount returns the row count`` () =
    let cc = 8
    let g = Grid.create 3 cc "foo"
    Assert.Equal(cc, Grid.colCount g)


[<Fact>]
let ``rowCount returns the row count`` () =
    let rc = 10
    let g = Grid.create rc 4 "foo"
    Assert.Equal(rc, Grid.rowCount g)
