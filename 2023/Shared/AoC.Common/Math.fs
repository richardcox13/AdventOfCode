module AoC.Common.Math

/// Greatest Common Denominator
let gcd left right =
    // Euclid's algorithm.... recursive version
    let rec inner (l: int64) r =
        if r = 0L then
            l
        else
            inner r (l % r)
    inner left right

//let gcd left right=
//    // TODO refactor to recursive to avoid the mutation
//    let mutable a = left
//    let mutable b = right
//    while b <> 0L do
//        let t = b
//        b <- a % b
//        a <- t
//    a

/// Geatest Common Factor: Alias for <c>gcd</c>
let gcf left right
    = gcd left right

