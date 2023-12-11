module AoC.Common.Core

open System

[<CustomEquality>][<NoComparison>]
type Position =
    { Row: int; Col: int }
    override x.ToString() = $"({x.Row}, {x.Col})"
    override x.GetHashCode() = HashCode.Combine(x.Row, x.Col)
    override left.Equals right =
        match right with
        | :? Position as r when left.Row = r.Row && left.Col = r.Col
            -> true
        | _ -> false
