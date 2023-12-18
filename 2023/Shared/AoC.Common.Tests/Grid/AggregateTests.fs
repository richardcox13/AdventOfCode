module AoC.Common.Tests.Grid.AggregateTests

open System
open AoC.Common
open AoC.Common.Core
open Xunit

[<Fact>]
let ``sumBy sums with identity function wors for simple case`` () =
    let rc = 57
    let cc = 8
    let g = Grid.create rc cc 1

    let sum = Grid.sumBy identity g
    Assert.Equal(cc * rc, sum)


[<Fact>]
let ``sumBy sums with a project correctly calculates`` () =
    let rc = 129
    let cc = 47
    let g = Grid.create rc cc "2"

    let sum = Grid.sumBy (fun s -> Int32.Parse(s)) g
    Assert.Equal(cc * rc * 2, sum)

