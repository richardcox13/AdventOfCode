namespace AoC.Common

type grid<'t> = 't array2d

module Grid = 

    open System

    let public create<'t> rows cols (value: 't) =
        ArgumentOutOfRangeException.ThrowIfLessThanOrEqual(rows, 0)
        ArgumentOutOfRangeException.ThrowIfLessThanOrEqual(cols, 0)

        Array2D.create rows cols value

    let colCount (g: 't grid) =
        Array2D.length2 g

    let maxCol (g: 't grid) =
        (Array2D.length2 g) - 1

    let maxRow (g: 't grid) =
        (Array2D.length1 g) - 1

    let rowCount (g: 't grid) =
        Array2D.length1 g

    let sumBy<'T> (project: 'T -> int) (g: 'T grid) =
        seq {
            let mr = maxRow g
            let mc = maxCol g
            for r in 0 .. mr do
                for c in 0 .. mc do
                    yield g[r,c] |> project
        }
        |> Seq.sum
