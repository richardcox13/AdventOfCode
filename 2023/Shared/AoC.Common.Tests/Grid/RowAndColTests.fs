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
let ``maxCol returns maximum row index`` () =
    let cc = 17
    let g = Grid.create 10 cc '~'
    Assert.Equal(cc-1, Grid.maxCol g)

[<Fact>]
let ``maxRow returns the maximum row index`` () =
    let rc = 10
    let g = Grid.create rc 4 "foo"
    Assert.Equal(rc-1, Grid.maxRow g)

[<Fact>]
let ``rowCount returns the row count`` () =
    let rc = 10
    let g = Grid.create rc 4 "foo"
    Assert.Equal(rc, Grid.rowCount g)
