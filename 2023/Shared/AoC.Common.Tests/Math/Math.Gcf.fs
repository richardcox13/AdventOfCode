module Math.Gcf

open AoC.Common
open Xunit

[<Fact>]
let ``GCF 9L 12L is 3L`` () =
    let res = Math.gcf 9L 12L
    Assert.Equal(3L, res)

