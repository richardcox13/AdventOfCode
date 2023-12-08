module Math.Gcf

open AoC.Common
open Xunit

[<Fact>]
let ``GCD 9L 12L is 3L`` () =
    let res = Math.gcd 9L 12L
    Assert.Equal(3L, res)


[<Fact>]
let ``GCD 7L 2L is 1L`` () =
    let res = Math.gcd 7L 3L
    Assert.Equal(1L, res)


[<Fact>]
let ``GCD 28L 7L is 7L`` () =
    let res = Math.gcd 28L 7L
    Assert.Equal(7L, res)

[<Fact>]
let ``GCD 60L 25L is 5L`` () =
    let res = Math.gcd 60L 5L
    Assert.Equal(5L, res)

[<Fact>]
let ``GCF is the same as GCD`` () =
    let res = Math.gcf 60L 5L
    Assert.Equal(5L, res)
