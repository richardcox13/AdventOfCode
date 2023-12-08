module AoC.Common.Math

// Greatest Common Factor
let gcf left right=
    // Euclid's algorithm.
    // TODO refactor to recursive to avoid the mutation
    let mutable a = left
    let mutable b = right
    while b <> 0L do
        let t = b
        b <- a % b
        a <- t
    a


